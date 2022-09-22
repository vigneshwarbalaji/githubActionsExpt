package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.objectify.LocalDatastoreExtension;
import com.yoco.commons.dataservices.objectify.ObjectifyExtension;
import com.yoco.commons.entity.UserClockinSubStatusJDO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.List;

import static com.yoco.commons.dataservices.objectify.OfyService.ofy;

@ExtendWith({
        LocalDatastoreExtension.class,
        ObjectifyExtension.class})
class ActivityImplTest {

    ActivityImpl activity = ActivityImpl.getActivityImplInstance();

    @Test
    void getActivityImplInstance_test(){
        Assertions.assertEquals(ActivityImpl.class,ActivityImpl.getActivityImplInstance().getClass());
    }

    @Test
    void saveActivity_test(){
        Assertions.assertEquals(null,activity.saveActivity(null));
    }

    @Test
    void saveActivities_test(){
        UserClockinSubStatusJDO activity1 = new UserClockinSubStatusJDO();
        activity1.setUserClockInSubStatusID("id1");
        activity1.setUniquepin("uniquepin");
        activity1.setContactId("contactId");
        activity1.setEventLongTime(1660638783000l);

        UserClockinSubStatusJDO activity2 = new UserClockinSubStatusJDO();
        activity2.setUserClockInSubStatusID("id2");
        activity2.setUniquepin("uniquepin");
        activity2.setContactId("contactId");
        activity2.setEventLongTime(1660638785000l);

        activity.saveActivities(List.of(activity1,activity2));

        Assertions.assertEquals(List.of(activity1,activity2),activity.getActivityForRange("uniquepin","contactId",1660627109000l,1660640595000l,"",10,"false").getItems());

        ofy().delete().entities(List.of(activity1, activity2)).now();
    }
}
