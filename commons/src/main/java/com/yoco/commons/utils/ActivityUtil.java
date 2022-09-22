package com.yoco.commons.utils;

import com.yoco.commons.dataservices.impl.ActivityImpl;
import com.yoco.commons.entity.UserClockinSubStatusJDO;
import lombok.NoArgsConstructor;

import java.util.List;

public class ActivityUtil {

    @NoArgsConstructor
    public enum ACTIVITIES {

        DUMMY("DUMMY");

        private String value;

        ACTIVITIES(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    private static void persist(UserClockinSubStatusJDO activityToSave){
        ActivityImpl.getActivityImplInstance().saveActivity(activityToSave);
    }

    public static void saveActivity(String accountId,String contactId,String projectId,String emailId,String activity,String activityType,long receivedLongTime){
        var activityToSave = new UserClockinSubStatusJDO(accountId,contactId,projectId,emailId,activity,activityType,receivedLongTime);
        persist(activityToSave);
    }

    public static void saveActivity(String accountId,String contactId,String projectId,String emailId,String activity,String activityType){
        var activityToSave = new UserClockinSubStatusJDO(accountId,contactId,projectId,emailId,activity,activityType,DateUtil.getCurrentTime());
        persist(activityToSave);
    }

    public static UserClockinSubStatusJDO saveActivity(UserClockinSubStatusJDO objUserClockinSubStatusJDO, long receivedLongTime){
        objUserClockinSubStatusJDO.setReceivedLongTime(receivedLongTime);
        return ActivityImpl.getActivityImplInstance().saveActivity(objUserClockinSubStatusJDO);
    }

    public static void saveActivities(List<UserClockinSubStatusJDO> activitiesList){
        ActivityImpl.getActivityImplInstance().saveActivities(activitiesList);
    }

}
