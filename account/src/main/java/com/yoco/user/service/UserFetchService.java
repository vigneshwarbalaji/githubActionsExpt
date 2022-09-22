package com.yoco.user.service;

import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.utils.DcmUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.CommonConstants;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import java.io.IOException;
import java.lang.reflect.Array;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Slf4j
@Service
public class UserFetchService {

    public Map<String,Object> getUser(String accountID, String contactID){
        Validator.checkArgument( ObjUtils.isNullOrEmpty( accountID ), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value() );
        Validator.checkArgument( ObjUtils.isNullOrEmpty( contactID ), COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value() );

        Map<String,Object> userMap = new HashMap<>();

        PeopleRelationJDO userPro = UserPROUtil.getUserProWithContact(accountID, contactID);

        if(!ObjUtils.isNull(userPro)){
            log.info(" is a valid PRO ");
            var convertedUser = new UserDTO(userPro,null);
            userMap.put(CommonConstants.USER_KEY,convertedUser);
        }
        return userMap;
    }

    public Map<String,Object> getAccessPolicy(String accountID,String contactID){
        Validator.checkArgument( ObjUtils.isNullOrEmpty( accountID ), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value() );
        Validator.checkArgument( ObjUtils.isNullOrEmpty( contactID ), COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value() );

        Map<String,Object> userPermissionMap = new HashMap<>();

        PeopleRelationJDO userPro = UserPROUtil.getUserPro(accountID, contactID);

        if(!ObjUtils.isNull(userPro)){
            log.info("PRO is valid, proceeding to fetch permissions");
            userPermissionMap = new AccessManager().getPolicy(userPro);
        }

        return userPermissionMap;
    }

    public Map<String,Object> getAllUsers(String accountID, Boolean isDelete, int limit, String cursor){
        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value() );

        Map<String, Object> allUsersMap =  new HashMap<>();
        Map<String, Object> allUsersPro = UserImpl.getUserImplInstance().getAllUsers(accountID,isDelete,limit,cursor);
        List<UserDTO> respProList = new ArrayList<>();

        if(!ObjUtils.isNullOrEmpty(allUsersPro)) {

            if (!ObjUtils.isNullOrEmpty((List<PeopleRelationJDO>) allUsersPro.get(CommonConstants.USERS_KEY))) {

                List<PeopleRelationJDO> allUserProList = (ArrayList<PeopleRelationJDO>) allUsersPro.get(CommonConstants.USERS_KEY);
                log.info("PRO size : " + allUserProList.size());

                respProList = new UserDTO().generateUserDTOList(allUserProList);
            }

            if (!ObjUtils.isNullOrEmpty((String) allUsersPro.get(CommonConstants.CURSOR))) {
                allUsersMap.put(CommonConstants.CURSOR, allUsersPro.get(CommonConstants.CURSOR));
            }
        }

        allUsersMap.put(CommonConstants.USERS_KEY,respProList);
        return allUsersMap;
    }

    public Map<String, Object> getAllUsersWithOutContact(String accountID) {
        Map<String, Object> resp = new HashMap<>();

        List<PeopleRelationJDO> usersList = UserImpl.getUserImplInstance().getAllUsers(accountID, false);

        if (!usersList.isEmpty()) {
            Map<String, Object> userMap = new HashMap<>();
            for (PeopleRelationJDO pro : usersList) {
                if (!ObjUtils.isNull(pro)) {
                    userMap.put(pro.getContactId(), new UserDTO(pro,null));
                }
            }
            resp.put(CommonConstants.USERS_KEY, userMap);
        }

        return resp;
    }

    public Map<String, Object> getRecentlyUpdatedPROs(String accountID, long dateModified) {

        Map<String, Object> responseMap = new HashMap<>();

        List<PeopleRelationJDO> usersList = UserImpl.getUserImplInstance().getRecentlyUpdatedPRO(accountID, dateModified);

        List<UserDTO> userDTOList = new UserDTO().generateUserDTOList(usersList);

        if(!userDTOList.isEmpty()) {
            responseMap.put(CommonConstants.USERS_KEY, userDTOList);
        }

        return responseMap;
    }

    public Map<String, Object> getMeInfo(String accountID, Contact userContact ) throws NoSuchAlgorithmException, IOException {
        String userContactID = userContact.getId();
        PeopleRelationJDO userPro = UserPROUtil.DEFAULT_ACCOUNT_KEY.equalsIgnoreCase(accountID) ?
                UserPROUtil.validateAndExtractDefaultUserPRO(userContactID) : UserPROUtil.validateAndExtractUserPRO(accountID, userContactID);
        Validator.checkArgument(ObjUtils.isNull(userPro), USER_ERROR_MESSAGE.USER_NOT_FOUND.value());
        userContact.setIsPasswordPresent(DcmUtil.getIsPasswordPresentForUserEmailID(userContact.getEmailID()));
        userPro.setContact(userContact);
        HashMap<String,Object> response = new HashMap<>();
        response.put(CommonConstants.USER_KEY, new UserDTO(userPro , UserPROUtil.getUserPermissions(userPro.getUniquepin(),userContactID)));
        return response;
    }

    public Map<String, Object> getAllUsersInAccount(String accountID, String cursor){
        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Map<String, Object> users = new UserImpl().getAllUsers(accountID, null, 2000, cursor);
        if(!ObjUtils.isNullOrEmpty(users)) {
            List<PeopleRelationJDO> usersList = (ArrayList)users.get("users");
            List<UserDTO> userDTOList = usersList.stream().map(user-> new UserDTO(user, null)).collect(Collectors.toList());
            users.put("users", userDTOList);
        }
        return users;
    }
}