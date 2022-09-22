package com.yoco.contact.service;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.CommonConstants;
import com.yoco.contact.helper.ProfileHelper;
import com.yoco.user.service.UserSkillService;
import org.springframework.stereotype.Service;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;


@Service
public class ProfileService {

    public Map<String,Object> getProfileImageUploadUrl(String contactId,String fileName) throws NoSuchAlgorithmException, IOException {

        return UrlFetcher.sendPostRequest(DcmConstants.getFetchProfileImageUrl(),Map.of("contactID",contactId,"title",fileName),
                HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(), UrlFetcher.getHttpClientInstance());
    }

    public Map<String,Object> updateProfileImage(String payload) throws NoSuchAlgorithmException, IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        Map<String,Object> dcmResp = UrlFetcher.sendPostRequest(DcmConstants.getUpdateProfileImageUrl(),payload,
                HeaderUtil.getContentTypedServerTokenWithUserAgentHeader(), UrlFetcher.getHttpClientInstance());

        if(Boolean.TRUE.equals(dcmResp.get(Commons.SUCCESS))){

            Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);

            PeopleRelationJDO userPro = UserPROUtil.getUserPro((String) payloadMap.get("accountID"),(String) payloadMap.get("contactID"));
            if(ObjUtils.isNull(userPro)){
                return Map.of();
            }

            String updatedProfileImg = (String) payloadMap.get("profile_pic_url");
            String[] newUpdatedProfileImg = updatedProfileImg.split("\\?",0);

            UserDTO userDTO = ProfileHelper.getInstance().profileImageUpdateHandler(userPro,newUpdatedProfileImg[0].trim());

            if(!ObjUtils.isNull(userDTO)){
                return Map.of(CommonConstants.USER_KEY,userDTO);
            }
        }

        return Map.of();
    }


    public Map<String,Object> updateProfile(Contact requesterContact, String payload) throws NoSuchAlgorithmException, IOException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload) || !JsonUtil.isValidJson(payload),COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        String accountID = (String) payloadMap.get(CommonConstants.ACCOUNT_ID_KEY);
        String contactID = (String) payloadMap.get(CommonConstants.CONTACT_ID_KEY);

        boolean isContactUpdateForSelf = requesterContact.getId().equalsIgnoreCase(contactID);
        PeopleRelationJDO userPro = UserPROUtil.getUserPro(accountID, requesterContact.getId());

        Validator.checkArgument((Boolean.FALSE.equals(isContactUpdateForSelf) && !UserPROUtil.isUserAnAdminInAccount(userPro)),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());

        ProfileHelper profileHelper = ProfileHelper.getInstance();

        Map<String,Object> dcmResp = profileHelper.updateProfileInDcm(payloadMap,accountID,contactID);

        if(!ObjUtils.isNullOrEmpty(dcmResp) && Boolean.TRUE.equals(dcmResp.get(Commons.SUCCESS))){

            List<Map<String,Object>> contactList = (List<Map<String, Object>>) dcmResp.get(ContactConstants.CONTACT);

            final Map<String,Object> contact = contactList.get(0);

            if(Boolean.FALSE.equals(isContactUpdateForSelf)){
                userPro = UserPROUtil.getUserPro(accountID,contactID);
            }

            UserDTO userDTO = profileHelper.profileUpdateHandler(userPro, contact);

            if(!ObjUtils.isNull(userDTO)){
                return Map.of(UserSkillService.UPDATED_PRO,userDTO);
            }
        }

        return Map.of();
    }

}
