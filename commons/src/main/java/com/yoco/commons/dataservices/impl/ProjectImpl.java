package com.yoco.commons.dataservices.impl;

import com.googlecode.objectify.cmd.Query;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.dataservices.dao.ProjectDao;
import com.yoco.commons.utils.ObjUtils;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;

@Slf4j
public class ProjectImpl extends OfyService implements ProjectDao {

    @NoArgsConstructor
    private enum PROJECT_IMPLEMENTATION_CONSTANTS {

        IS_DELETED("isDeleted"),
        CURSOR("cursor"),
        PROJECTS("projects"),
        UNIQUE_PIN("uniquepin");

        private String value;

        PROJECT_IMPLEMENTATION_CONSTANTS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public static ProjectImpl getProjectImplInstance() {
        return new ProjectImpl();
    }

    @Override
    public ProjectJDO saveProject(ProjectJDO projectJDO) {
        save(projectJDO);
        return projectJDO;
    }

    @Override
    public ProjectJDO getProject(String id) {

        return get(ProjectJDO.class, id);

    }

    @Override
    public Map<String, Object> getAllProjectsInAccount(String accountID, String type, String cursor, int limit) {

        Map<String, Object> responseMap = new HashMap<>();

        Query<ProjectJDO> query = ofy().load().type(ProjectJDO.class)
                .filter(PROJECT_IMPLEMENTATION_CONSTANTS.UNIQUE_PIN.value(),accountID);

        if(type.equals("inactive")){
            query = query.filter(PROJECT_IMPLEMENTATION_CONSTANTS.IS_DELETED.value(), true);
        }else if(type.equals("active")){
            query = query.filter(PROJECT_IMPLEMENTATION_CONSTANTS.IS_DELETED.value(), false);
        }

        CollectionResponse<ProjectJDO> response = fetchCursorQuery(query, limit,cursor);

        responseMap.put(PROJECT_IMPLEMENTATION_CONSTANTS.PROJECTS.value(), response.getItems());

        if (!ObjUtils.isNullOrEmpty(response.getCursor()))
            responseMap.put(PROJECT_IMPLEMENTATION_CONSTANTS.CURSOR.value(), response.getCursor());

        return responseMap;

    }

}
