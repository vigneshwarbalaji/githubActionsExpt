package com.yoco.contact.helper;

import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.CommonConstants;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class ProfileHelper {

    public static ProfileHelper getInstance(){
        return new ProfileHelper();
    }

    public UserDTO profileImageUpdateHandler(PeopleRelationJDO userPro,String photoId) throws IOException {
        ContactImpl contactImpl = ContactImpl.getContactImplInstance();
        Contact contact = contactImpl.getByID(userPro.getContactId());
        if(ObjUtils.isNull(contact)){
            return null;
        }

        String existingPhotoId = contact.getPhotoID();
        String activity = "Contact update from DCM :"+contact.getEmailID()+" contactID : "+contact.getId()+". Details are : contact photoID : "+existingPhotoId+" DCM photoID : " + photoId;

        if(existingPhotoId.equalsIgnoreCase(photoId)){
            log.info(" photo is updated via full hook, so not proceeding further to update again ... ");
            return null;
        }
        contact.setPhotoID(photoId);
        return this.saveContactAndInitiateQueue(contact,contactImpl,userPro,activity);
    }


    public Map<String,Object> updateProfileInDcm(Map<String,Object> payloadMap, String accountID, String contactID) throws NoSuchAlgorithmException, IOException {

        String firstName = Validator.sanitizeText((String)payloadMap.get(ContactConstants.FIRST_NAME));
        String lastName = Validator.sanitizeText((String)payloadMap.get(ContactConstants.LAST_NAME));

        Map<String, Object> requestPayLoad = new HashMap<>();

        if(!ObjUtils.isNullOrEmpty(firstName))
            requestPayLoad.put(ContactConstants.FIRST_NAME,firstName.trim());

        if(!ObjUtils.isNullOrEmpty(lastName))
            requestPayLoad.put(ContactConstants.LAST_NAME,lastName.trim());

        Validator.checkArgument(ObjUtils.isNullOrEmpty(requestPayLoad), COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());

        requestPayLoad.put(ContactConstants.ID,contactID);

        return DcmUtil.updateContactInDcm(accountID,requestPayLoad);
    }

    public UserDTO profileUpdateHandler(PeopleRelationJDO userPro, Map<String,Object> dcmContact) throws IOException {
        ContactImpl contactImpl = ContactImpl.getContactImplInstance();
        Contact contact = contactImpl.getByID(userPro.getContactId());
        if(ObjUtils.isNull(contact)){
            return null;
        }
        String existingFirstName = contact.getFirstName();
        String existingLastName = contact.getLastName();
        var activity = new StringBuilder();
        var isProfileChanged = false;

        if(!existingFirstName.equalsIgnoreCase((String) dcmContact.get(ContactConstants.FIRST_NAME))){
            contact.setFirstName((String) dcmContact.get(ContactConstants.FIRST_NAME));
            activity.append("contact FirstName : "+existingFirstName+" DCM FirstName : "+ dcmContact.get(ContactConstants.FIRST_NAME));
            isProfileChanged = true;
        }

        if(!existingLastName.equalsIgnoreCase((String) dcmContact.get(ContactConstants.LAST_NAME))){
            contact.setLastName((String) dcmContact.get(ContactConstants.LAST_NAME));
            activity.append(" contact LastName : "+existingLastName+" DCM LastName : "+ dcmContact.get(ContactConstants.LAST_NAME));
            isProfileChanged = true;
        }

        if(!isProfileChanged) { // have to remove this check after full hook refactoring
            log.info(" contact info might be updated via hook, added this check to avoid re-updating...");
            return null;
        }

        return this.saveContactAndInitiateQueue(contact,contactImpl,userPro,activity.toString());
    }


    public UserDTO saveContactAndInitiateQueue(Contact contact,ContactImpl contactImpl,PeopleRelationJDO userPro,String activity){
        contact.setDateModifiedLongTime(DateUtil.getCurrentTime());
        contactImpl.saveContact(contact);
        log.info(" contact table is updated ... ");
        return this.initiateProfileUpdateQueue(contact, userPro, activity);
    }

    public UserDTO initiateProfileUpdateQueue(Contact contact, PeopleRelationJDO userPro, String activity){
        try{
            userPro.setContact(contact);
            UserDTO userDTO = new UserDTO(userPro,null);

            ContactTaskInitiator.initiateContactUpdateOperationsTaskQueue(
                    Map.of( CommonConstants.USER_KEY,userDTO,
                            CommonConstants.ACTION,"profileUpdate",
                            CommonConstants.ACTIVITY, activity
                    )
            );
            return userDTO;
        }catch (Exception e){
            log.info(" exception in initiateProfileUpdateQueue : " + e.getMessage());
            return null;
        }
    }

}
