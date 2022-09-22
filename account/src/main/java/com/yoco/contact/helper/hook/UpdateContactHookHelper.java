package com.yoco.contact.helper.hook;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.enums.Status;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.utils.*;
import com.yoco.contact.helper.ProfileHelper;
import com.yoco.user.helper.staff.UserStaffHelper;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class UpdateContactHookHelper {


    public static UpdateContactHookHelper getInstance(){
        return new UpdateContactHookHelper();
    }

    public void processContactUpdation(DcmContactDTO dcmContactDTO, Contact contact) {
        List<PeopleRelationJDO> userPros = UserImpl.getUserImplInstance().getAllUserProsForUser(dcmContactDTO.getId(),null);
        this.validateAndProcessProfileInfoUpdation(dcmContactDTO, contact,userPros);
        this.validateAndProcessProsAssociation(dcmContactDTO,contact,userPros);
    }

    public void validateAndProcessProfileInfoUpdation(DcmContactDTO dcmContactDTO, Contact contact,List<PeopleRelationJDO> userPros)  {

        try{
            var isProfileInfoUpdated = false;
            var isEmailUpdated = false;
            var activity = new StringBuilder();
            activity.append("Contact update from DCM. Details are : ");

            if(!ObjUtils.isNullOrEmpty(dcmContactDTO.getLogin()) && !contact.getEmailID().equalsIgnoreCase(dcmContactDTO.getLogin())){
                activity.append("contact emailID : "+contact.getEmailID()+" DCM emailID : "+dcmContactDTO.getLogin() + " ");
                contact.setEmailID(dcmContactDTO.getLogin());
                isProfileInfoUpdated = true;
                isEmailUpdated = true;
            }

            if(!ObjUtils.isNullOrEmpty(dcmContactDTO.getFirstName()) && !contact.getFirstName().equalsIgnoreCase(dcmContactDTO.getFirstName())){
                activity.append("contact FirstName : "+contact.getFirstName()+" DCM FirstName : "+ dcmContactDTO.getFirstName() + " ");
                contact.setFirstName(dcmContactDTO.getFirstName());
                isProfileInfoUpdated = true;
            }

            if(!contact.getLastName().equalsIgnoreCase(dcmContactDTO.getLastName())){
                activity.append("contact LastName : "+contact.getLastName()+" DCM LastName : "+ dcmContactDTO.getLastName()+ " ");
                contact.setLastName(dcmContactDTO.getLastName());
                isProfileInfoUpdated = true;
            }

            if(!ObjUtils.isNull(dcmContactDTO.getPhotoID()) && !contact.getPhotoID().equalsIgnoreCase(dcmContactDTO.getPhotoID())){
                activity.append("contact photoID : "+contact.getPhotoID()+" DCM photoID : "+dcmContactDTO.getPhotoID());
                contact.setPhotoID(dcmContactDTO.getPhotoID());
                isProfileInfoUpdated = true;
            }

            if(isProfileInfoUpdated)
                this.processProfileInfoUpdation(dcmContactDTO, contact, userPros, activity.toString(), isEmailUpdated);
            else
                log.info(" there is no profile information updation for user " + dcmContactDTO.getLogin());

        }catch (Exception e){
          log.error(" exception in handleProfileInfoUpdation: " + e.getMessage());
        }
    }

    public void processProfileInfoUpdation(DcmContactDTO dcmContactDTO, Contact contact, List<PeopleRelationJDO> userPros,
                                            String activity, boolean isEmailUpdated) {

        log.info("processing to update profile information of user...");

        if (isEmailUpdated){
            this.updateUserPROsWithMailID(dcmContactDTO.getLogin(),userPros);
            this.updateSettingsJdoForPrimaryUsersWithMailID(dcmContactDTO);
        }

        contact.setDateModifiedLongTime(DateUtil.getCurrentTime());
        ContactImpl.getContactImplInstance().saveContact(contact);

        List<PeopleRelationJDO> activePros = userPros.stream()
                .filter(pro -> Boolean.FALSE.equals(pro.isDelete()))
                .collect(Collectors.toList());

        log.info(" activePros size: " + activePros.size());

        if(!ObjUtils.isNullOrEmpty(activePros)){
            ProfileHelper profileHelper = ProfileHelper.getInstance();
            activePros.stream().forEach(pro -> profileHelper.initiateProfileUpdateQueue(contact, pro, activity));
        }

    }

    private List<PeopleRelationJDO> updateUserPROsWithMailID(String updatedEmailID, List<PeopleRelationJDO> userPros) {
        userPros.stream().forEach(pro -> {
            pro.setEmailID(updatedEmailID);
            pro.setDateModified(DateUtil.getCurrentTime());
        });
        UserImpl.getUserImplInstance().savePros(userPros);
        return userPros;
    }

    public void updateSettingsJdoForPrimaryUsersWithMailID(DcmContactDTO dcmContactDTO){
        try{
            List<AccessPolicy> userPolicies = AccessManager.getAllActiveAccessPoliciesOfUser(dcmContactDTO.getId());

            log.info(" user policies : " + (ObjUtils.isNull(userPolicies) || userPolicies.isEmpty() ? 0 :userPolicies.size()));

            if (!ObjUtils.isNullOrEmpty(userPolicies)) {

                List<String> superAdminsAccountIDs = userPolicies.stream()
                        .filter(policy -> policy.getPermissions().contains(IAMPermission.SUPER_ADMIN.toString()))
                        .map(policy -> policy.getResource().split("/")[1])
                        .collect(Collectors.toList());

                if (!ObjUtils.isNullOrEmpty(superAdminsAccountIDs)) {
                    AccountImpl accountImpl = AccountImpl.getAccountImplInstance();
                    List<SettingsJDO> accountsList = accountImpl.getAccounts(superAdminsAccountIDs);

                    if(!ObjUtils.isNullOrEmpty(accountsList)){
                        accountsList.stream().forEach(account -> account.setSourceEmail(dcmContactDTO.getLogin()));
                        accountImpl.saveAccounts(accountsList);
                    }
                }
            }

        }catch (Exception e){
            log.error(" exception in updateSettingsJdoForPrimaryMailUpdate " + e.getMessage());
        }
    }

    public void validateAndProcessProsAssociation(DcmContactDTO dcmContactDTO, Contact contact, List<PeopleRelationJDO> userPros){
        try{

            CreateContactHookHelper contactHookHelper = CreateContactHookHelper.getInstance();

            List<SettingsJDO> accounts = contactHookHelper.extractValidAccounts(dcmContactDTO.getLinkedAccounts());
            log.info(" user has any valid dcm accounts? : " + (ObjUtils.isNull(accounts) || accounts.isEmpty() ? 0 :accounts.size()));

            if(!ObjUtils.isNullOrEmpty(accounts)){

                List<PeopleRelationJDO> prosToActivate = this.validateAndExtractProsToActivate(userPros, accounts);
                log.info(" user has any pros to reactivate ? : " + (ObjUtils.isNull(prosToActivate) || prosToActivate.isEmpty() ? 0 :prosToActivate.size()));

                if(!ObjUtils.isNullOrEmpty(prosToActivate)){
                    this.processProsActivation(contact, accounts, prosToActivate);
                }

                List<SettingsJDO> prosToCreate = contactHookHelper.validateAndExtractProsToCreate(userPros, accounts);
                log.info(" user has any pros to create ? : " + (ObjUtils.isNull(prosToCreate) ||prosToCreate.isEmpty() ? 0 :prosToCreate.size()));

                if(!ObjUtils.isNullOrEmpty(prosToCreate)){
                    this.processProsCreation(contact,prosToCreate,contactHookHelper);
                }

            }
        }catch (Exception e){
            log.error(" exception in validateAndReactivateContactInAccount helper :: " + e.getMessage());
        }
    }

    public List<PeopleRelationJDO> validateAndExtractProsToActivate(List<PeopleRelationJDO> userPros,List<SettingsJDO> accounts) {
        return userPros.stream()
                .filter(pro -> Boolean.TRUE.equals(pro.isDelete()) &&
                        accounts.stream()
                        .anyMatch(account -> Status.ACTIVE.toString().equalsIgnoreCase(account.getStatus())
                                && account.getPeopleUniquePin().equals(pro.getUniquepin())))
                .collect(Collectors.toList());
    }

    public void processProsActivation(Contact contact, List<SettingsJDO> accounts, List<PeopleRelationJDO> proActivationList) {
        try {

            String defaultAccountId = DcmUtil.getDefaultAccountIDWithEmailID(contact.getEmailID());
            log.info(" defaultAccountId from DCM to identify default flag to enable: " + defaultAccountId);

            UserImpl userImpl = UserImpl.getUserImplInstance();
            UserStaffHelper userStaffHelper = UserStaffHelper.getInstance();

            PeopleRelationJDO defaultUserPro = userImpl.getDefaultUserPro(contact.getId());

            proActivationList.stream().forEach(pro -> {

                if(pro.getUniquepin().equalsIgnoreCase(defaultAccountId))
                    pro.setDefault(true);

                pro.setDelete(false);
                userStaffHelper.activateUserSkillHelper(pro);
                pro.setContact(contact);

                SettingsJDO accountJdo = extractAccountJdo(accounts, pro);
                if(!ObjUtils.isNull(accountJdo)){
                    userStaffHelper.activateUserEventsHandler(accountJdo, pro,
                            "MailID " + pro.getEmailID() + " is not active, so reactivated it back in this domain "
                                    + pro.getUniquepin(), ClientSource.CLIENT_SOURCE_CONSTANTS.HOOK.value());
                }

            });

            this.validateAndSetDefaultFlag(defaultAccountId, userImpl, defaultUserPro);

        } catch (Exception e) {
            log.info("Exception in processProsActivation : " + e.getMessage());
        }

    }

    private void validateAndSetDefaultFlag(String defaultAccountId, UserImpl userImpl, PeopleRelationJDO defaultUserPro) {
        if (!ObjUtils.isNull(defaultUserPro) && !defaultUserPro.getUniquepin().equalsIgnoreCase(defaultAccountId)) {
            log.info(" default user pro :  " + defaultUserPro.getUniquepin());
            defaultUserPro.setDefault(false);
            defaultUserPro.setDateModified(DateUtil.getCurrentTime());
            userImpl.savePro(defaultUserPro);
        }
    }

    private SettingsJDO extractAccountJdo(List<SettingsJDO> accounts, PeopleRelationJDO pro) {
        return accounts.stream()
                .filter(account -> account.getPeopleUniquePin().equalsIgnoreCase(pro.getUniquepin()))
                .findFirst().orElse(null);
    }

    public void processProsCreation(Contact contact, List<SettingsJDO> accounts, CreateContactHookHelper contactHookHelper) throws NoSuchAlgorithmException, IOException {
        String defaultAccountId = DcmUtil.getDefaultAccountIDWithEmailID(contact.getEmailID());

        UserImpl userImpl = UserImpl.getUserImplInstance();
        PeopleRelationJDO defaultUserPro = userImpl.getDefaultUserPro(contact.getId());

        if(!ObjUtils.isNullOrEmpty(defaultAccountId)) {
            contactHookHelper.processPROCreation(accounts, defaultAccountId, contact,
                    contact.getId() + " has  been added by YoCo_Update_FullSync.");
            this.validateAndSetDefaultFlag(defaultAccountId, userImpl, defaultUserPro);
        }
    }
}
