package com.yoco.contact.helper.hook;

import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.Status;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.contact.helper.ContactHelper;
import com.yoco.user.helper.staff.UserStaffHelper;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class CreateContactHookHelper {

    public static CreateContactHookHelper getInstance(){
        return new CreateContactHookHelper();
    }

    public List<SettingsJDO> extractValidAccounts(List<String> dcmLinkedAccounts){
        if(ObjUtils.isNullOrEmpty(dcmLinkedAccounts)) {
            return List.of();
        }
        List<SettingsJDO> accounts = AccountImpl.getAccountImplInstance().getAccounts(dcmLinkedAccounts);
        return ObjUtils.isNull(accounts) ? List.of() : accounts;
    }

    public List<SettingsJDO> extractProCreationList(List<SettingsJDO> accounts,String contactId){

        List<PeopleRelationJDO> userPros = UserImpl.getUserImplInstance().getAllUserProsForUser(contactId,null);

        if(!ObjUtils.isNull(userPros)){
            log.info(" Filtering the accounts that are required to create new workspaces ...");
            return this.validateAndExtractProsToCreate(userPros,accounts);
        }

        return accounts;
    }

    public List<SettingsJDO> validateAndExtractProsToCreate(List<PeopleRelationJDO> userPros,List<SettingsJDO> accounts){
        return accounts.stream()
                .filter(account -> Status.ACTIVE.toString().equalsIgnoreCase(account.getStatus()) &&
                        userPros.stream()
                                .noneMatch(pro -> account.getPeopleUniquePin().equals(pro.getUniquepin())))
                .collect(Collectors.toList());
    }

    public void processContactCreation(DcmContactDTO dcmContactDTO) throws NoSuchAlgorithmException,IOException {

        String contactId = dcmContactDTO.getId();

        String defaultAccountId = DcmUtil.getDefaultAccountIDWithEmailID(dcmContactDTO.getLogin());

        if(!ObjUtils.isNullOrEmpty(defaultAccountId)){

            log.info(" defaultAccountId from DCM : " + defaultAccountId);

            List<SettingsJDO> accounts = this.extractValidAccounts(dcmContactDTO.getLinkedAccounts());

            if(!ObjUtils.isNullOrEmpty(accounts)){

                accounts = this.extractProCreationList(accounts,contactId);

                var contactCreated = ContactHelper.getInstance().validateAndCreateContact(dcmContactDTO);

                String activityMessage = this.buildActivityMessage(contactCreated);
                this.processPROCreation(accounts, defaultAccountId, contactCreated, activityMessage);
            }
        }
    }

    public void processPROCreation(List<SettingsJDO> accounts, String defaultAccountId, Contact contactCreated, String activityMessage) {
        UserStaffHelper staffHelper = UserStaffHelper.getInstance();

        accounts.stream().forEach(account -> {
            PeopleRelationJDO userObj = staffHelper.createUserPRO(account,defaultAccountId.equalsIgnoreCase(account.getPeopleUniquePin()),
                    contactCreated,"");
            staffHelper.createUserEventsHandler(null,account,null,activityMessage,userObj);
        });
    }

    public String buildActivityMessage(Contact contactCreated){
        StringBuilder activityBuilder = new StringBuilder();
        activityBuilder.append("Contact created for emailID :: "+contactCreated.getEmailID()+" contactID : "
                +contactCreated.getId()+" Details are :Firstname :: " + contactCreated.getFirstName());

        if (!ObjUtils.isBlank(contactCreated.getLastName()))
            activityBuilder.append(", Lastname :: " + contactCreated.getLastName());

        if (!ObjUtils.isBlank(contactCreated.getPhotoID()))
            activityBuilder.append(", Photo exists");

        activityBuilder.append(".");
        return activityBuilder.toString();
    }

}
