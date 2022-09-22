package com.yoco.user.service;

import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.user.helper.UserDownloadHelper;
import com.yoco.user.helper.UserProProfileHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class UserService {

    public Map<String,Object> updateProfilePRO(String accountID, String contactID, String payload) throws IOException {

        Validator.checkArgument( ObjUtils.isNullOrEmpty(payload) || !JsonUtil.isValidJson(payload),
                COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        PeopleRelationJDO userPRO = UserPROUtil.getUserProWithContact(accountID,contactID);
        Validator.checkArgument(ObjUtils.isNull(userPRO),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());

        Map<String,Object> resp = new HashMap<>();

        Map<String, Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        var userDTO = UserProProfileHelper.getInstance().updateProfilePRO(payloadMap,userPRO);

        if(!ObjUtils.isNull(userDTO)){
            resp.put(UserSkillService.UPDATED_PRO, userDTO);
        }

        return resp;
    }

    public Map<String,Object> downloadStaffData(String requesterContactID, String accountID, String status, String outputFields){
        Validator.checkArgument(!UserPROUtil.isUserAnAdminInAccount(accountID,requesterContactID),COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        Boolean staffStatusFilterValue = (Boolean) UserDownloadHelper.getIsDeleteQueryValue(status);
        List<PeopleRelationJDO> staffList = UserPROUtil.getAllUsersInCompany(accountID,staffStatusFilterValue);
        UserPROUtil.sortProListBasedOnEmail(staffList);
        String reportCsv = UserDownloadHelper.generateStaffDataCsv(staffList, outputFields);
        String fileName = UserDownloadHelper.generateStaffDataCsvFileName(accountID,staffStatusFilterValue);
        HashMap<String,Object> response = new HashMap<>();
        response.put("fileName",fileName);
        response.put("report",reportCsv);
        return response;
    }
}