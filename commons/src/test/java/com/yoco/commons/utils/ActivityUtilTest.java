package com.yoco.commons.utils;

import com.yoco.commons.dataservices.impl.ActivityImpl;
import com.yoco.commons.entity.UserClockinSubStatusJDO;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.times;

class ActivityUtilTest {

    @Test
    void saveActivity_test(){
        try(MockedStatic<ActivityImpl> activityMockedStatic = Mockito.mockStatic(ActivityImpl.class)){
           ActivityImpl activityMock =  Mockito.mock(ActivityImpl.class);
           Mockito.when(activityMock.saveActivity(any(UserClockinSubStatusJDO.class))).thenReturn(new UserClockinSubStatusJDO());
           activityMockedStatic.when(ActivityImpl::getActivityImplInstance).thenReturn(activityMock);
           ActivityUtil.saveActivity("accountId","contactId","","test@gmail.com","activity",ActivityUtil.ACTIVITIES.DUMMY.value(), 0L);
           Mockito.verify(activityMock,times(1)).saveActivity(any(UserClockinSubStatusJDO.class));
        }
    }

    @Test
    void saveActivity_without_longTime_test(){
        try(MockedStatic<ActivityImpl> activityMockedStatic = Mockito.mockStatic(ActivityImpl.class)){
            ActivityImpl activityMock =  Mockito.mock(ActivityImpl.class);
            Mockito.when(activityMock.saveActivity(any(UserClockinSubStatusJDO.class))).thenReturn(new UserClockinSubStatusJDO());
            activityMockedStatic.when(ActivityImpl::getActivityImplInstance).thenReturn(activityMock);
            ActivityUtil.saveActivity("accountId","contactId","","test@gmail.com","activity",ActivityUtil.ACTIVITIES.DUMMY.value());
            Mockito.verify(activityMock,times(1)).saveActivity(any(UserClockinSubStatusJDO.class));
        }
    }

    @Test
    void saveActivity_with_receivedTime_test(){
        try(MockedStatic<ActivityImpl> activityMockedStatic = Mockito.mockStatic(ActivityImpl.class)){
            ActivityImpl activityMock =  Mockito.mock(ActivityImpl.class);
            Mockito.when(activityMock.saveActivity(any(UserClockinSubStatusJDO.class))).thenReturn(new UserClockinSubStatusJDO());
            activityMockedStatic.when(ActivityImpl::getActivityImplInstance).thenReturn(activityMock);
            var activityToSave = new UserClockinSubStatusJDO("accountId","contactId","","test@gmail.com","activity",ActivityUtil.ACTIVITIES.DUMMY.value(),DateUtil.getCurrentTime());
            ActivityUtil.saveActivity(activityToSave,0l);
            Mockito.verify(activityMock,times(1)).saveActivity(any(UserClockinSubStatusJDO.class));
        }
    }

    @Test
    void saveActivities_test(){
        try(MockedStatic<ActivityImpl> activityMockedStatic = Mockito.mockStatic(ActivityImpl.class)){
            ActivityImpl activityMock =  Mockito.mock(ActivityImpl.class);
            Mockito.doNothing().when(activityMock).saveActivities(anyList());
            activityMockedStatic.when(ActivityImpl::getActivityImplInstance).thenReturn(activityMock);
            var activityToSave1 = new UserClockinSubStatusJDO("accountId","contactId","","test@gmail.com","activity1",ActivityUtil.ACTIVITIES.DUMMY.value(),DateUtil.getCurrentTime());
            var activityToSave2 = new UserClockinSubStatusJDO("accountId","contactId","","test@gmail.com","activity2",ActivityUtil.ACTIVITIES.DUMMY.value(),DateUtil.getCurrentTime());
            List<UserClockinSubStatusJDO> activitiesList = List.of(activityToSave1,activityToSave2);
            ActivityUtil.saveActivities(activitiesList);
            Mockito.verify(activityMock,times(1)).saveActivities(activitiesList);
        }
    }

}
