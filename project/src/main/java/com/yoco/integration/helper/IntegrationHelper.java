package com.yoco.integration.helper;

import com.fullauth.api.model.oauth.OauthAccessToken;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.IntegrationsImpl;
import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.integration.modal.ZendeskEntry;
import com.yoco.integration.modal.ZendeskRunningEntry;
import java.util.*;

public class IntegrationHelper {

    private static final String SESSIONS_KEY = "sessions";
    private static final String DAYLEVELDURATION_KEY = "dayLevelDuration";

    private IntegrationHelper(){}
    public static boolean isAccessAllowed(OauthAccessToken requesterAccessToken, String userContactID){
        if(ObjUtils.isNull(requesterAccessToken) || ObjUtils.isNullOrEmpty(userContactID)){
            return false;
        }
        return AnnotationHelper.isAccessTokenTypeServer(requesterAccessToken) || userContactID.equalsIgnoreCase(requesterAccessToken.getUserId());
    }

    public static String extractIntegrationDetails(Map<String,Object> payload){
        if(ObjUtils.isNullOrEmpty(payload)){
            return "{}";
        }
        String integrationDetailsJson = (String) payload.get("integrationDetails");
        if(ObjUtils.isNullOrEmpty(integrationDetailsJson)){
            return "{}";
        }
        return integrationDetailsJson;
    }

    public static IntegrationsJDO disableIntegration(IntegrationsImpl integrationsImpl, IntegrationsJDO userIntegration){
        if(ObjUtils.isNull(userIntegration)){
            return null;
        }
        integrationsImpl.delete(userIntegration);
        userIntegration.setIntegrationDetails("{}");
        userIntegration.setIsActive(false);
        return userIntegration;
    }

    public static IntegrationsJDO updateIntegration(IntegrationsImpl integrationsImpl,IntegrationsJDO userIntegration,String newIntegrationDetails){
        if(ObjUtils.isNull(userIntegration)){
            return null;
        }
        userIntegration.setIntegrationDetails(newIntegrationDetails);
        integrationsImpl.saveIntegration(userIntegration);
        return userIntegration;
    }

    public static IntegrationsJDO createIntegration(IntegrationsImpl integrationsImpl, PeopleRelationJDO userPro, String newIntegrationDetails, String integrationType){
        var userIntegration = new IntegrationsJDO(userPro.getUniquepin(),userPro.getContactId(),integrationType,newIntegrationDetails,true);
        integrationsImpl.saveIntegration(userIntegration);
        return userIntegration;
    }

    public static Map<String,Object> getEntriesMapForTaskID(List<Map<String,Object>> userEntriesList, String timezone){
        Map<String,Object> responseMap = new HashMap<>();
        if(ObjUtils.isNullOrEmpty(userEntriesList)){
            return responseMap;
        }
        sortEventsBasedOnStartTimeDesc(userEntriesList);
        Object runningSession = getRunningSession(userEntriesList.get(0), timezone);
        var isFirstEntryRunning = true;
        if(ObjUtils.isNull(runningSession)){
            isFirstEntryRunning = false;
            runningSession = new HashMap<>();
        }
        Map<String,Object> entriesMap = new HashMap<>();
        Long totalDuration = 0L;
        for (int i = isFirstEntryRunning ? 1 : 0 ; i < userEntriesList.size(); i++){
            var entry = new ZendeskEntry(userEntriesList.get(i),timezone);
            totalDuration += entry.getDuration();
            updateEntriesMap(entriesMap,entry);
        }
        responseMap.put("runningSession",runningSession);
        responseMap.put("totalDuration",totalDuration);
        responseMap.put("entriesMap",entriesMap);
        return responseMap;
    }

    public static void updateEntriesMap(Map<String,Object> entriesMap, ZendeskEntry entry){
        String key = entry.getInDate();
        Map<String,Object> dayLevelMap = new HashMap<>();
        List<ZendeskEntry> sessionsList = new ArrayList<>();
        Long dayLevelDuration = 0L;
        if(entriesMap.containsKey(key)){
            dayLevelMap = (Map<String, Object>) entriesMap.get(key);
            sessionsList = (List<ZendeskEntry>) dayLevelMap.get(SESSIONS_KEY);
            dayLevelDuration = (Long) dayLevelMap.get(DAYLEVELDURATION_KEY);
        }
        sessionsList.add(entry);
        dayLevelDuration += entry.getDuration();
        dayLevelMap.put(SESSIONS_KEY,sessionsList);
        dayLevelMap.put(DAYLEVELDURATION_KEY,dayLevelDuration);
        entriesMap.put(key,dayLevelMap);
    }

    public static ZendeskRunningEntry getRunningSession(Map<String,Object> firstEntry, String timezone){
        if(ObjUtils.isNullOrEmpty(firstEntry) || !ReportsUtil.isRunningEntry(firstEntry)){
            return null;
        }
        firstEntry.put(SchedulingKeys.END_TIME,new Date().getTime());
        return new ZendeskRunningEntry(firstEntry,timezone);
    }

    public static void sortEventsBasedOnStartTimeDesc(List<Map<String,Object>> events){
        if(ObjUtils.isNullOrEmpty(events)){
            return;
        }
        events.sort((entry1, entry2) -> (int) ((Long) entry2.get(SchedulingKeys.START_TIME) - (Long) entry1.get(SchedulingKeys.START_TIME)));
    }
}
