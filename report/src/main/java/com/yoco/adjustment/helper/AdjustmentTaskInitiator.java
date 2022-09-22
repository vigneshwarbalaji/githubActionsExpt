package com.yoco.adjustment.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static com.yoco.constants.EventConstants.ADJUSTMENTS;
import static com.yoco.constants.EventConstants.ENTRIES;

public class AdjustmentTaskInitiator {
    private AdjustmentTaskInitiator(){}
    private static final String ADJUSTMENTS_PROCESS_QUEUE = "process-adj-events";

    private static String getAdjustmentDeleteHandlerUrl(){
        return CommonAppProperties.getAppUrl() + "/task/adjustment/delete-handler";
    }

    private static String getApproveAdjustmentTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/adjustment/approve";
    }

    private static String getEditApprovedAdjustmentTaskCallBackUrl() {
        return CommonAppProperties.getAppUrl() + "/task/adjustment/edit-approve";
    }

    public static void initiateDeleteAdjustmentTaskQueue(List<ReportsDTO> revertedEntries, List<AdjustmentDTO> deletedAdjustments, PeopleRelationJDO entryContactPro, PeopleRelationJDO loggedInUserPro) throws IOException {
        TaskCreator.createPostTask(ADJUSTMENTS_PROCESS_QUEUE,getAdjustmentDeleteHandlerUrl(),
                CloudTaskUtil.convertObjectToByteArray(Map.of(ENTRIES,revertedEntries, ADJUSTMENTS,deletedAdjustments,"entryContactPro",entryContactPro,"loggedInUserPro",loggedInUserPro)));
    }

    public static void initiateApproveAdjustmentQueue(PeopleRelationJDO adminPro, Map<String,Object> eventMap) throws IOException {
        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put("adminPRO",adminPro);
        payloadMap.put("eventMap",eventMap);
        TaskCreator.createPostTask(ADJUSTMENTS_PROCESS_QUEUE,getApproveAdjustmentTaskCallBackUrl(),
                CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }

    public static void initiateEditApprovedAdjustmentQueue(PeopleRelationJDO adminPro, Map<String,Object> eventMap,Map<String,Object> entryTimeDetailsMap) throws IOException {
        Map<String,Object> payloadMap = new HashMap<>();
        payloadMap.put("adminPRO",adminPro);
        payloadMap.put("eventMap",eventMap);
        payloadMap.put("newAdjustmentTimeDetailsInfoMap",entryTimeDetailsMap);
        TaskCreator.createPostTask(ADJUSTMENTS_PROCESS_QUEUE,getEditApprovedAdjustmentTaskCallBackUrl(),
                CloudTaskUtil.convertObjectToByteArray(payloadMap));
    }
}
