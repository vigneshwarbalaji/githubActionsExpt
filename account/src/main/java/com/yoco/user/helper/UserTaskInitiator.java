package com.yoco.user.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.CloudTaskUtil;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.Map;

@Slf4j
public class UserTaskInitiator {

    private UserTaskInitiator(){}

    private static final String PRO_UPDATE_OPERATIONS_QUEUE = "updateUserPro";
    private static final String STAFF_OPERATIONS_QUEUE = "staff-operation";

    private static String getProUpdateTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/user/proUpdateTaskHandler";
    }

    private static String getStaffTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/user/staffTaskHandler";
    }

    private static String getUserDeletedTaskHandlerUrl(){
        return CommonAppProperties.getAppUrl() + "/task/user/delete";
    }

    public static void initiateProUpdateOperationsTaskQueue(Map<String,Object> proObject) throws IOException {
        log.info(" proUpdate task queue " + proObject);
        TaskCreator.createPostTask(PRO_UPDATE_OPERATIONS_QUEUE,getProUpdateTaskCallBackUrl(),CloudTaskUtil.convertObjectToByteArray(proObject));
    }

    public static void initiateStaffOperationsTaskQueue(Map<String,Object> staffObject) throws IOException {
        TaskCreator.createPostTask(STAFF_OPERATIONS_QUEUE,getStaffTaskCallBackUrl(),CloudTaskUtil.convertObjectToByteArray(staffObject));
    }

    public static void initiateDeleteUserQueue(PeopleRelationJDO adminPRO, PeopleRelationJDO userPro,String source) throws IOException {
        Map<String,Object> payloadMap = Map.of("adminPro", adminPRO, "userPro", userPro,"requestSource",source);
        TaskCreator.createPostTask(STAFF_OPERATIONS_QUEUE,getUserDeletedTaskHandlerUrl(),CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }

}
