package com.yoco.commons.dataservices.dao;

import com.yoco.commons.entity.UserClockinSubStatusJDO;
import com.yoco.commons.modal.CollectionResponse;

import java.util.List;

public interface ActivityDao {

    UserClockinSubStatusJDO saveActivity(UserClockinSubStatusJDO activity);

    CollectionResponse<UserClockinSubStatusJDO> getActivityForRange(String accountID, String contactID, Long startMilliseconds,
                                                                    Long endMilliseconds, String  cursor , int limit, String isPrevious);

    void saveActivities(List<UserClockinSubStatusJDO> activitiesList);
}
