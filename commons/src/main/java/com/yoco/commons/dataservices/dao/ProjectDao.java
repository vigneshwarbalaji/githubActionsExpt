package com.yoco.commons.dataservices.dao;

import com.yoco.commons.entity.ProjectJDO;
import java.util.Map;

public interface ProjectDao {

    ProjectJDO saveProject(ProjectJDO projectJDO);

    ProjectJDO getProject (String id);

    Map<String, Object> getAllProjectsInAccount(String accountID, String type, String cursor, int limit);

}
