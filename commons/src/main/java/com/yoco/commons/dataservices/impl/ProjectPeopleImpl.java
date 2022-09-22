package com.yoco.commons.dataservices.impl;

import com.googlecode.objectify.cmd.Query;
import com.yoco.commons.entity.ProjectPeopleJDO;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.dataservices.dao.ProjectPeopleDao;
import com.yoco.commons.dataservices.objectify.OfyService;
import lombok.NoArgsConstructor;

import java.util.List;

public class ProjectPeopleImpl extends OfyService implements ProjectPeopleDao {

    @NoArgsConstructor
    private enum PROJECT_PEOPLE_CONSTANTS {
        IS_DELETED("isDeleted"),
        ACTIVE("active"),
        INACTIVE("inActive"),
        PROJECT_ID("projectId"),
        PEOPLE_ID("peopleId"),
        UNIQUE_PIN("uniquepin");
        private String value;
        PROJECT_PEOPLE_CONSTANTS(String value) {
            this.value = value;
        }
        public String value() {
            return value;
        }
    }

    public static ProjectPeopleImpl getProjectPeopleImplInstance(){
        return new ProjectPeopleImpl();
    }

    @Override
    public List<ProjectPeopleJDO> saveProjectAssociations(List<ProjectPeopleJDO> projectPeopleList) {
        ofy().save().entities(projectPeopleList).now();
        return projectPeopleList;
    }

    @Override
    public List<ProjectPeopleJDO> getProjectAssociationByProjectID(String accountID, String projectID) {
        return ofy().load().type(ProjectPeopleJDO.class)
                .filter(PROJECT_PEOPLE_CONSTANTS.UNIQUE_PIN.value(), accountID)
                .filter(PROJECT_PEOPLE_CONSTANTS.PROJECT_ID.value(), projectID).list();
    }

    @Override
    public CollectionResponse<ProjectPeopleJDO> getProjectsAssociatedToUser(String accountID, String contactID, String cursor, int limit) {
        Query<ProjectPeopleJDO> query = ofy().load().type(ProjectPeopleJDO.class)
                                                 .filter(PROJECT_PEOPLE_CONSTANTS.UNIQUE_PIN.value(), accountID)
                                                 .filter(PROJECT_PEOPLE_CONSTANTS.PEOPLE_ID.value(), contactID);
        return fetchCursorQuery( query, limit, cursor);
    }

    @Override
    public ProjectPeopleJDO getProjectsAssociated(String accountID, String contactID, String projectID) {

        return ofy().load().type( ProjectPeopleJDO.class)
                .filter(PROJECT_PEOPLE_CONSTANTS.UNIQUE_PIN.value(), accountID)
                .filter( PROJECT_PEOPLE_CONSTANTS.PEOPLE_ID.value(), contactID)
                .filter( PROJECT_PEOPLE_CONSTANTS.PROJECT_ID.value(), projectID).first().now();
    }

    @Override
    public List<ProjectPeopleJDO> deleteProjectAssociations(List<ProjectPeopleJDO> projectPeopleList) {
        ofy().delete().entities(projectPeopleList);
        return projectPeopleList;
    }

    @Override
    public CollectionResponse<ProjectPeopleJDO> getAllProjectAssociationsUnderAccount(String accountID, int limit, String cursor){
        Query<ProjectPeopleJDO> query = ofy().load().type(ProjectPeopleJDO.class)
                .filter(PROJECT_PEOPLE_CONSTANTS.UNIQUE_PIN.value(),accountID);
        return fetchCursorQuery(query, limit, cursor);
    }
}
