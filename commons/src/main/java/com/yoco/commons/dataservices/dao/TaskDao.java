package com.yoco.commons.dataservices.dao;

import com.yoco.commons.entity.TaskJDO;

public interface TaskDao {

    TaskJDO getEntryByID (String id);
    void deleteTask(TaskJDO objTask);
    TaskJDO saveTask(TaskJDO objTask);
}
