package com.yoco.commons.dataservices.impl;

import com.googlecode.objectify.cmd.Query;
import com.yoco.commons.dataservices.dao.ActivityDao;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.UserClockinSubStatusJDO;
import com.yoco.commons.modal.CollectionResponse;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@Slf4j
public class ActivityImpl extends OfyService implements ActivityDao {

    public static ActivityImpl getActivityImplInstance(){
        return new ActivityImpl();
    }

    @Override
    public UserClockinSubStatusJDO saveActivity(UserClockinSubStatusJDO activity) {
        save(activity);
        return activity;
    }

    @Override
    public CollectionResponse<UserClockinSubStatusJDO> getActivityForRange(String accountID, String contactID, Long startMilliseconds, Long endMilliseconds , String cursor , int limit, String isPrevious){

        Query<UserClockinSubStatusJDO> query    =       ofy().load().type(UserClockinSubStatusJDO.class).filter("uniquepin", accountID)
                .filter("contactId",contactID);

        if("true".equalsIgnoreCase(isPrevious)){
            query = query.filter("eventLongTime <", startMilliseconds)
                    .order("-eventLongTime");
        } else{
            query = query.filter("eventLongTime >=",startMilliseconds).filter("eventLongTime <=",endMilliseconds)
                    .order("eventLongTime");
        }

        return fetchCursorQuery(query,limit,cursor);

    }


    @Override
    public void saveActivities(List<UserClockinSubStatusJDO> activitiesList) {
        saveCollection(activitiesList);
    }

}
