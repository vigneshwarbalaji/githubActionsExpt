package com.yoco.user.service;

import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.modal.user.ProUserInfo;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.*;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.CommonConstants;
import com.yoco.user.helper.staff.UserStaffDCMHelper;
import com.yoco.user.helper.UserTaskInitiator;
import com.yoco.user.helper.staff.UserStaffHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

@Slf4j
@Service
public class UserStaffService {

    public static final String CONTACT_LIST = "contactList";

    public static UserStaffService getInstance(){
        return new UserStaffService();
    }

    public void validateAdminAuthentication(String accountID,Contact loggedInContact){
        PeopleRelationJDO adminPro = UserPROUtil.getUserPro(accountID,loggedInContact.getId());
        Validator.checkArgument( !UserPROUtil.isUserAnAdminInAccount(adminPro),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
    }

    public Map<String,Object> createUser(String accountID, Contact currentUser, String reqPayload,boolean shouldPerformAdminCheck) throws NoSuchAlgorithmException, IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(reqPayload) || !JsonUtil.isValidJson(reqPayload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value() );

        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(reqPayload);
        var userStaffHelper = UserStaffHelper.getInstance();

        userStaffHelper.validateUserPayload(payloadMap);

        if(shouldPerformAdminCheck)
            this.validateAdminAuthentication(accountID,currentUser);

        Map<String,Object> resp = userStaffHelper.validateUserAccountCheck(accountID,payloadMap);

        if(Boolean.TRUE.equals(resp.get(Commons.SUCCESS))){
            log.info("No user in yoco board processing to create one. ");

            var emailID = resp.get(ContactConstants.EMAIL_ID).toString();
            SettingsJDO account = (SettingsJDO) resp.get(CommonConstants.ACCOUNT_KEY);

            Map<String,Object> dcmResp = UserStaffDCMHelper.getInstance().validateAndExtractContactFromDCM(emailID,accountID,payloadMap);

            return userStaffHelper.processUserCreation(dcmResp,currentUser,account);
        }
        return resp;
    }


    public Map<String,Object> createUserPRO(String accountID, String contactID){
        Map<String,Object> responseMap = new HashMap<>();
        SettingsJDO accountObj = AccountImpl.getAccountImplInstance().getById(accountID);

        if(!ObjUtils.isNull(accountObj)){
            PeopleRelationJDO userPro = UserPROUtil.getUserPro(accountID,contactID);

            if(ObjUtils.isNull(userPro)){
                PeopleRelationJDO primaryAdminPRO = UserPROUtil.getPrimaryAdmin(accountID);

                if(!ObjUtils.isNull(primaryAdminPRO)){
                    log.info(" fetched primary admin pro ");
                    var contact = ContactImpl.getContactImplInstance().getByID(contactID);

                    if(!ObjUtils.isNull(contact)){
                        log.info(" fetched user contact info ");

                        boolean isAccountPayrollEnabled = accountObj.getIsPayrollEnabled();
                        var proUserInfo = new ProUserInfo(accountID,contactID,contact.getEmailID(), Skillset.ROLE_STAFF,"","");
                        var userObj   =   new PeopleRelationJDO(proUserInfo,primaryAdminPRO.getContactId(),accountObj.getTimeZone(),
                                accountObj.getTimeZoneDisplayName(), true, isAccountPayrollEnabled);

                        log.info(" userObj " + userObj);

                        userObj = UserImpl.getUserImplInstance().savePro(userObj);
                        userObj.setContact(contact);

                        var accessPolicy = AccessManager.getInstance().createNewPolicyForMember(accountID,contactID,isAccountPayrollEnabled);
                        responseMap.put(CommonConstants.USER_KEY,userObj);
                        responseMap.put(CommonConstants.POLICY,accessPolicy);
                    }
                }
            }else{
                responseMap.put(Commons.MESSAGE,"user already exists");
            }
        }
        return responseMap;
    }

    public Map<String,Object> importUser(String accountID, Contact currentUser, String payload) throws IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload) || !JsonUtil.isValidJson(payload),
                COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value() );

        Map<String,Object> jsonMap = JsonUtil.convertJsonToMap(payload);
        Validator.checkArgument( !jsonMap.containsKey(CONTACT_LIST), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value() );

        this.validateAdminAuthentication(accountID,currentUser);

        Map<String, Object> responseMap = new HashMap<>();

        List<Map<String,Object>> contactList = (ArrayList<Map<String,Object>>) jsonMap.get(CONTACT_LIST);

        if(!ObjUtils.isNullOrEmpty(contactList)){

            Map<String,Object> queueMap = new HashMap<>();
            queueMap.put(CommonConstants.ACTION,"staffImported");
            queueMap.put(CommonConstants.ACCOUNT_ID_KEY,accountID);
            queueMap.put(ContactConstants.CONTACT,currentUser);
            queueMap.put(CONTACT_LIST,contactList);

            UserTaskInitiator.initiateStaffOperationsTaskQueue(queueMap);

            responseMap.put("message", "Mail will be sent to you shortly");
        }

        return responseMap;
    }

    public Map<String,Object> activateUser(String accountID,String contactID,Contact currentUser) throws NoSuchAlgorithmException, IOException {

        Map<String, Object> responseMap =   new HashMap<>();

        SettingsJDO account = AccountImpl.getAccountImplInstance().getById(accountID);

        if(!ObjUtils.isNull(account)){

            this.validateAdminAuthentication(accountID,currentUser);

            PeopleRelationJDO userPro = UserPROUtil.getUserProWithContact(accountID,contactID);

            if(!ObjUtils.isNull(userPro) && Boolean.TRUE.equals(userPro.isDelete())) {

                userPro.setDelete(false);

                var userStaffHelper = UserStaffHelper.getInstance();

                userPro = userStaffHelper.validateAndSetIsDefault(contactID, userPro);
                userPro = userStaffHelper.activateUserSkillHelper(userPro);

                DcmUtil.linkContactToAccount(accountID,contactID);

                String activityMessage  = contactID + " has been enabled by " + currentUser.getId();
                UserDTO userDTO = userStaffHelper.activateUserEventsHandler(account, userPro, activityMessage, ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());

                responseMap.put(CommonConstants.USER_KEY, userDTO);
            }

        }

        return responseMap;
    }


    public Map<String, Object> deleteUser(String accountID, String contactID, String requesterContactID) throws IOException {
        var adminPRO = UserPROUtil.validateAndExtractUserPRO(accountID,requesterContactID);
        Validator.checkArgument(!UserPROUtil.isUserAnAdminInAccount(adminPRO),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        var userPro = UserPROUtil.validateAndExtractUserPROWithContact(accountID,contactID);
        Validator.checkArgument(ObjUtils.isNull(userPro),COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
        Validator.checkArgument(UserPROUtil.isPrimaryAdmin(userPro),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        UserStaffHelper.getInstance().updateAndSaveProForUserDisabling(userPro);
        UserTaskInitiator.initiateDeleteUserQueue(adminPRO, userPro,"web");
        return Map.of(CommonConstants.USER_KEY,new UserDTO(userPro,null));
    }
}
