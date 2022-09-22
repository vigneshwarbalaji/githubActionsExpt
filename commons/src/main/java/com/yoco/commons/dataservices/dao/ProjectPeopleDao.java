package com.yoco.commons.dataservices.dao;

import com.yoco.commons.entity.ProjectPeopleJDO;
import com.yoco.commons.modal.CollectionResponse;

import java.util.List;

public interface ProjectPeopleDao {

    List<ProjectPeopleJDO> saveProjectAssociations(List<ProjectPeopleJDO> projectPeopleList);

    List<ProjectPeopleJDO> getProjectAssociationByProjectID(String accountID, String projectID);

    CollectionResponse<ProjectPeopleJDO> getProjectsAssociatedToUser(String accountID, String contactID, String cursor, int limit);

    ProjectPeopleJDO getProjectsAssociated(String accountID, String contactID, String projectID);

    List<ProjectPeopleJDO> deleteProjectAssociations(List<ProjectPeopleJDO> projectPeopleList);

    CollectionResponse<ProjectPeopleJDO> getAllProjectAssociationsUnderAccount(String accountID, int limit, String cursor);

}
