package com.yoco.user.helper.staff;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.enums.error.DCM_ERROR_RESPONSE;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;


@Slf4j
public class UserStaffDCMHelper {

    public static UserStaffDCMHelper getInstance(){
        return new UserStaffDCMHelper();
    }

    public Map<String,Object> validateAndExtractContactFromDCM(String emailID,String accountID,Map<String,Object> payload) throws NoSuchAlgorithmException, IOException {

        Map<String,Object> responseMap = new HashMap<>();

        Map<String,Object> dcmRespContact = DcmUtil.fetchContactFromDcm(emailID);

        log.info(" dcmResp " + dcmRespContact);

        if(dcmRespContact != null && !dcmRespContact.isEmpty()){

            boolean isContactExist = (boolean) dcmRespContact.get(ContactConstants.CONTACT_EXIST);

            if(isContactExist){
                log.info("contact exists in DCM so adding user account to DCM and to YOCO");
                responseMap = linkContactWithAccountInDcm(accountID,dcmRespContact);
            }else{
                log.info("contact is not existing in DCM so creating one.");
                responseMap = createNewContactInDcm(accountID,payload);
            }
        }else{
            responseMap.put(Commons.SUCCESS, Boolean.FALSE);
            responseMap.put(Commons.ERROR_RESPONSE, "Failed to upload the user, Please re-upload the user");
        }

        return responseMap;
    }

    public Map<String,Object> linkContactWithAccountInDcm(String accountID,Map<String,Object> dcmRespContact){
        Map<String,Object> responseMap = new HashMap<>();

        Map<String, Object> contactMap = (HashMap<String, Object>) dcmRespContact.get(ContactConstants.CONTACT);

        var dcmContact = new DcmContactDTO(contactMap);

        try {

            var defaultUniquePin = DcmUtil.getActiveDefaultAccountFromDcm(dcmContact.getLogin());
            log.info(" defaultUniquePin from dcm:  " + defaultUniquePin);

            Map<String, Object> contactLinkResp = DcmUtil.linkContactToAccount(accountID,dcmContact.getId());
            log.info(" contactLinkedResp: " + contactLinkResp);

            if(!ObjUtils.isNullOrEmpty(contactLinkResp) && (boolean)contactLinkResp.get(Commons.SUCCESS)){
                var markAsDefault = true;

                if (ObjUtils.isNullOrEmpty(defaultUniquePin)) {
                    log.info(" DefaultUnique Pin is empty so none is default, setting present domain as default with unique pin :  "
                            + accountID + "\n and  contact id : " + dcmContact.getId());
                    DcmUtil.updateDefaultAccountInDcm(accountID, dcmContact.getId());

                } else {
                    markAsDefault = false;
                    log.info("User is already having default domain with uniquePin, so not updating in dcm ");
                }

                responseMap.put(Commons.SUCCESS, Boolean.TRUE);
                responseMap.put("markAsDefault", markAsDefault);
                responseMap.put("isNewUser", !dcmContact.isPasswordPresent());
                responseMap.put("dcmContact",dcmContact);
            } else{
                responseMap.put(Commons.SUCCESS, Boolean.FALSE);
                responseMap.put(Commons.ERROR_RESPONSE,DCM_ERROR_RESPONSE.NOT_ALLOWED_IN_DCM.value());
            }
        } catch (Exception e) {
           log.info("exception in linking contact with account in DCM " + e.getMessage());
            responseMap.put(Commons.SUCCESS, Boolean.FALSE);
            responseMap.put(Commons.ERROR_RESPONSE,DCM_ERROR_RESPONSE.NOT_ALLOWED_IN_DCM.value());
        }

        return responseMap;
    }

    public Map<String,Object> createNewContactInDcm(String accountID,Map<String,Object> payload) throws NoSuchAlgorithmException, IOException {

        Map<String, Object> responseMap = new HashMap<>();

        var emailID = payload.get(ContactConstants.EMAIL_ID).toString();
        String firstName = ObjUtils.isNullOrEmpty(Validator.sanitizeText(payload.get(ContactConstants.FIRST_NAME).toString()))
                ? "" : Validator.sanitizeText(payload.get(ContactConstants.FIRST_NAME).toString());
        String lastName = ObjUtils.isNullOrEmpty(Validator.sanitizeText(payload.get(ContactConstants.LAST_NAME).toString()))
                ? "": Validator.sanitizeText(payload.get(ContactConstants.LAST_NAME).toString());
        String photoID = ObjUtils.isNullOrEmpty(payload.get(ContactConstants.PHOTO_ID).toString())
                ? "" : payload.get(ContactConstants.PHOTO_ID).toString();

        Map<String, Object> newDCMContactResp = DcmUtil.createUserInDcm(accountID, emailID, "", firstName, lastName, photoID);

        log.info(" newDCMContactResp " + newDCMContactResp);

        if ((boolean) newDCMContactResp.get(Commons.SUCCESS)) {

            Map<String, Object> contactMap = (HashMap<String, Object>) newDCMContactResp.get(ContactConstants.CONTACT);

            var dcmContact = new DcmContactDTO(contactMap);

            DcmUtil.updateDefaultAccountInDcm(accountID, dcmContact.getId());

            responseMap.put(Commons.SUCCESS, Boolean.TRUE);
            responseMap.put("dcmContact",dcmContact);
            responseMap.put("isNewUser",true);
            responseMap.put("markAsDefault",true);
        } else {
            responseMap.put(Commons.SUCCESS, Boolean.FALSE);
            responseMap.put(Commons.ERROR_RESPONSE, DCM_ERROR_RESPONSE.NOT_ALLOWED_IN_DCM.value());
        }

        return responseMap;
    }
}