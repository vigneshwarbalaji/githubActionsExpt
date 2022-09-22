package com.yoco.hours.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.constants.EventConstants;
import java.io.IOException;
import java.util.Map;

public class HoursTaskInitiator {
    private HoursTaskInitiator(){}
    private static final String CLOCK_OPERATION_QUEUE = "clockoperation";

    private static String getEntryDeletionTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/entry/delete-handler";
    }

    public static void initiateDeleteEntryQueue(PeopleRelationJDO loggedInUserPro, Contact entryContact, Map<String, Object> deletedEventMap) throws IOException {
        Map<String,Object> payloadMap = Map.of("loggedInUserPro",loggedInUserPro,"entryContact",entryContact, EventConstants.ENTRY,deletedEventMap);
        TaskCreator.createPostTask(CLOCK_OPERATION_QUEUE,getEntryDeletionTaskCallBackUrl(), CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }
}
