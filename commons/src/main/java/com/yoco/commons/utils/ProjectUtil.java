package com.yoco.commons.utils;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.dataservices.impl.ProjectImpl;
import com.yoco.commons.dataservices.impl.ProjectPeopleImpl;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.entity.ProjectPeopleJDO;
import com.yoco.commons.modal.CollectionResponse;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class ProjectUtil {
    private ProjectUtil(){}
    private static final String ACTIVE = "active";
    private static final String PROJECTS_KEY = "projects";
    public static List<ProjectJDO> getAllProjectsForUserInAnAccount(String accountID, String contactID){
        if(ObjUtils.isNullOrEmpty(accountID) || ObjUtils.isNullOrEmpty(contactID)){
            return new ArrayList<>();
        }
        List<ProjectPeopleJDO> userProjectAssociations = getAllProjectAssociationsForUserInAnAccount(accountID,contactID);
        if(ObjUtils.isNullOrEmpty(userProjectAssociations)){
            return new ArrayList<>();
        }
        Set<String> associatedProjectIds = userProjectAssociations.stream().map(ProjectPeopleJDO::getProjectId).collect(Collectors.toSet());
        return ProjectImpl.getProjectImplInstance().get(ProjectJDO.class,associatedProjectIds);
    }

    public static List<ProjectPeopleJDO> getAllProjectAssociationsForUserInAnAccount(String accountID, String contactID){
        if(ObjUtils.isNullOrEmpty(accountID) || ObjUtils.isNullOrEmpty(contactID)){
            return new ArrayList<>();
        }
        var projectPeopleImpl = ProjectPeopleImpl.getProjectPeopleImplInstance();
        List<ProjectPeopleJDO> userProjectAssociations = new ArrayList<>();
        var isQueryIncomplete = true;
        CollectionResponse<ProjectPeopleJDO> projectPeopleCollection;
        var cursor = "";
        while(isQueryIncomplete){
            projectPeopleCollection  = projectPeopleImpl.getProjectsAssociatedToUser(accountID,contactID,cursor,1000);
            userProjectAssociations.addAll(projectPeopleCollection.getItems());
            if(!ObjUtils.isNullOrEmpty(projectPeopleCollection.getCursor())){
                cursor = projectPeopleCollection.getCursor();
            }else{
                isQueryIncomplete = false;
            }
        }
        return userProjectAssociations;
    }

    public static List<String> getAllProjectIdsAssociatedToUser(String accountID, String contactID){
        List<ProjectPeopleJDO> projectPeopleList = getAllProjectAssociationsForUserInAnAccount(accountID, contactID);
        if(!projectPeopleList.isEmpty()){
           return projectPeopleList.stream().map(ProjectPeopleJDO::getProjectId).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    public static void disassociateUserFromAllProjectsInAccount(String accountID, String contactID){
        List<ProjectPeopleJDO> userProjectAssociations = getAllProjectAssociationsForUserInAnAccount(accountID,contactID);
        if(!userProjectAssociations.isEmpty()){
            ProjectPeopleImpl.getProjectPeopleImplInstance().delete((List)userProjectAssociations);
        }
    }

    public static List<ProjectPeopleJDO> getAllProjectAssociationsUnderAccount(String accountID){
        if(ObjUtils.isNullOrEmpty(accountID)){
            return new ArrayList<>();
        }
        var projectPeopleImpl = ProjectPeopleImpl.getProjectPeopleImplInstance();
        List<ProjectPeopleJDO> userProjectAssociations = new ArrayList<>();
        var isQueryIncomplete = true;
        CollectionResponse<ProjectPeopleJDO> projectPeopleCollection;
        var cursor = "";
        while(isQueryIncomplete){
            projectPeopleCollection  = projectPeopleImpl.getAllProjectAssociationsUnderAccount(accountID,1000,cursor);
            userProjectAssociations.addAll(projectPeopleCollection.getItems());
            if(!ObjUtils.isNullOrEmpty(projectPeopleCollection.getCursor())){
                cursor = projectPeopleCollection.getCursor();
            }else{
                isQueryIncomplete = false;
            }
        }
        return userProjectAssociations;
    }

    public static void deleteAllProjectAssociationsUnderAccount(String accountID){
        List<ProjectPeopleJDO> projectPeopleAssociations = getAllProjectAssociationsUnderAccount(accountID);
        if(!ObjUtils.isNullOrEmpty(projectPeopleAssociations)){
            ProjectPeopleImpl.getProjectPeopleImplInstance().deleteProjectAssociations(projectPeopleAssociations);
        }
    }

    public static void archiveAllProjectsUnderAccount(String accountID) {
        List<ProjectJDO> projectsUnderAccount = getAllProjectsUnderAccount(accountID);
        if(!ObjUtils.isNullOrEmpty(projectsUnderAccount)){
            for(ProjectJDO project : projectsUnderAccount){
                project.setIsDeleted(true);
            }
            ProjectImpl.getProjectImplInstance().saveCollection(projectsUnderAccount);
        }
    }

    public static List<ProjectJDO> getAllProjectsUnderAccount(String accountID){
        if(ObjUtils.isNullOrEmpty(accountID)){
            return new ArrayList<>();
        }
        var projectImpl = ProjectImpl.getProjectImplInstance();
        List<ProjectJDO> projects = new ArrayList<>();
        var isQueryIncomplete = true;
        Map<String,Object> projectResponse;
        var cursor = "";
        while(isQueryIncomplete){
            projectResponse  = projectImpl.getAllProjectsInAccount(accountID,ACTIVE,cursor,1000);
            projects.addAll((List<ProjectJDO>)projectResponse.get(PROJECTS_KEY));
            if(!ObjUtils.isNullOrEmpty((String) projectResponse.get(Commons.CURSOR))){
                cursor = (String) projectResponse.get(Commons.CURSOR);
            }else{
                isQueryIncomplete = false;
            }
        }
        return projects;
    }
}
