package com.yoco.commons.dataservices.dao;

import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import java.util.List;
import java.util.Map;

public interface UserDao {
    PeopleRelationJDO getUserWithoutContact(String accountID, String contactID);
    PeopleRelationJDO getDefaultUserPro(String contactID);
    PeopleRelationJDO getDefaultUserProWithEmail(String emailID);
    PeopleRelationJDO savePro(PeopleRelationJDO userPro);
    void savePros(List<PeopleRelationJDO> userProList);
    PeopleRelationJDO getUserByEmailID(String accountID,String emailID);

    Map<String, Object> getAllUsers(String accountID, Boolean isDelete, int limit, String cursor);
    List<PeopleRelationJDO> getAllUsers(String accountID, Boolean includeDeleted );

    List<PeopleRelationJDO> getRecentlyUpdatedPRO (String accountID, long dateModified);
    List<PeopleRelationJDO> getUsersByRole(String accountID,String role);

    List<Contact> getContactsByKeys(List<PeopleRelationJDO> proList);

    List<PeopleRelationJDO> getAllUserProsForUser(String contactID, Boolean isDelete);
    List<PeopleRelationJDO> getAllUserProsForUserByEmail(String email, Boolean isDelete);

}
