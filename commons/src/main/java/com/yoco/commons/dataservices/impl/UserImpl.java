package com.yoco.commons.dataservices.impl;

import com.googlecode.objectify.cmd.Query;
import com.yoco.commons.dataservices.dao.UserDao;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.ObjUtils;
import lombok.extern.slf4j.Slf4j;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
public class UserImpl extends OfyService implements UserDao {

    public static final String UNIQUE_PIN = "uniquepin";
    public static final String CONTACT_ID = "contactId";
    public static final String EMAIL_ID = "emailID";
    public static final String IS_DEFAULT = "isDefault";
    public static final String IS_DELETE  = "isDelete";
    public static final String USERS = "users";
    public static final String CURSOR = "cursor";

    public static UserImpl getUserImplInstance(){
        return new UserImpl();
    }

    @Override
    public PeopleRelationJDO getUserWithoutContact(String accountID, String contactID){
        if(ObjUtils.isNullOrEmpty(accountID) || ObjUtils.isNullOrEmpty(contactID)){
            return null;
        }
        return ofy().load().type(PeopleRelationJDO.class)
                .filter(UNIQUE_PIN, accountID)
                .filter(CONTACT_ID, contactID)
                .first().now();
    }

    @Override
    public PeopleRelationJDO getDefaultUserPro( String contactID){
        if(ObjUtils.isNullOrEmpty(contactID)){
            return null;
        }
        return ofy().load().type( PeopleRelationJDO.class)
                .filter( CONTACT_ID, contactID)
                .filter(IS_DEFAULT, true).first().now();
    }

    @Override
    public PeopleRelationJDO getDefaultUserProWithEmail( String emailID){
        if(ObjUtils.isNullOrEmpty(emailID)){
            return null;
        }
        return ofy().load().type( PeopleRelationJDO.class)
                .filter( EMAIL_ID, emailID)
                .filter(IS_DEFAULT, true).first().now();
    }

    @Override
    public PeopleRelationJDO savePro(PeopleRelationJDO userPro) {
        save(userPro);
        return userPro;
    }

    @Override
    public void savePros(List<PeopleRelationJDO> userProList) {
        saveCollection(userProList);
    }

    @Override
    public PeopleRelationJDO getUserByEmailID(String accountID, String emailID) {
        if(ObjUtils.isNullOrEmpty(accountID) || ObjUtils.isNullOrEmpty(emailID)){
            return null;
        }
        return ofy().load().type(PeopleRelationJDO.class)
                .filter(UNIQUE_PIN, accountID)
                .filter(EMAIL_ID, emailID)
                .first().now();
    }

    @Override
    public List<PeopleRelationJDO> getAllUserProsForUserByEmail(String emailID, Boolean isDelete) {
        if(ObjUtils.isNullOrEmpty(emailID)){
            return List.of();
        }
        Query<PeopleRelationJDO> query = ofy().load().type( PeopleRelationJDO.class)
                .filter(EMAIL_ID, emailID);
        if(!ObjUtils.isNull(isDelete))
            query = query.filter(IS_DELETE, isDelete);
        return  query.list();
    }


    public List<Contact> getContactsByKeys(List<PeopleRelationJDO> proList){
        List<String> keys = proList.stream().map(PeopleRelationJDO::getContactId).collect(Collectors.toList());
        return get(Contact.class, keys);
    }

    @Override
    public List<PeopleRelationJDO> getAllUserProsForUser(String contactID, Boolean isDelete) {
        if(ObjUtils.isNullOrEmpty(contactID)){
            return List.of();
        }
        Query<PeopleRelationJDO> query = ofy().load().type( PeopleRelationJDO.class)
                .filter(CONTACT_ID, contactID);
        if(!ObjUtils.isNull(isDelete))
            query = query.filter(IS_DELETE, isDelete);
        return  query.list();
    }

    @Override
    public Map<String, Object> getAllUsers(String accountID, Boolean isDelete, int limit, String cursor) {
        Map<String, Object> responseMap = new HashMap<>();
        if(ObjUtils.isNullOrEmpty(accountID)){
            return responseMap;
        }
        limit    = limit == 0 ? 500 : limit;
        cursor   = ObjUtils.isBlank(cursor) ? "" : cursor;

        Query<PeopleRelationJDO> query = ofy().load().type( PeopleRelationJDO.class)
                                        .filter(UNIQUE_PIN, accountID);

        if(!ObjUtils.isNull(isDelete))
            query = query.filter(IS_DELETE, isDelete);

        CollectionResponse<PeopleRelationJDO> collectionResponse = fetchCursorQuery(query, limit, cursor);

        if(!ObjUtils.isNullOrEmpty(collectionResponse.getItems())){
            responseMap.put(USERS, collectionResponse.getItems());
        }

        if(!ObjUtils.isNullOrEmpty(collectionResponse.getCursor())) {
            responseMap.put(CURSOR, collectionResponse.getCursor());
        }

        return responseMap;
    }

    public Map<String, Object> getAllUsersWithContact(String accountID, Boolean isDelete, int limit, String cursor) {
        Map<String,Object> userResponse = getAllUsers(accountID,isDelete,limit,cursor);
        List<PeopleRelationJDO> usersList = (List<PeopleRelationJDO>) userResponse.get(USERS);
        if(!ObjUtils.isNullOrEmpty(usersList)){
            List<Contact> contacts = getContactsByKeys(usersList);
            Contact userContact;
            for(PeopleRelationJDO userPro : usersList){
                userContact = contacts.stream().filter(contact -> userPro.getContactId().equals(contact.getId())).findFirst().orElse(null);
                contacts.remove(userContact);
                userPro.setContact(userContact);
            }
        }
        return userResponse;
    }

    @Override
    public List<PeopleRelationJDO> getAllUsers(String accountID, Boolean includeDeleted) {
        Query<PeopleRelationJDO> query = ofy().load().type(PeopleRelationJDO.class)
                                            .filter(UNIQUE_PIN, accountID);

        if(Boolean.FALSE.equals(includeDeleted)){
            query = query.filter(IS_DELETE, false);
        }

        return query.list();
    }

    @Override
    public List<PeopleRelationJDO> getRecentlyUpdatedPRO(String accountID, long dateModified) {
        return ofy().load().type(PeopleRelationJDO.class)
                                                .filter(UNIQUE_PIN, accountID)
                                                .filter("dateModified >=", dateModified).list();
    }

    @Override
    public List<PeopleRelationJDO> getUsersByRole(String accountID, String role) {
        return ofy().load().type(PeopleRelationJDO.class)
                            .filter(UNIQUE_PIN, accountID)
                            .filter("role",role)
                            .filter(IS_DELETE, false).list();
    }

}
