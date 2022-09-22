package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.dao.TaskDao;
import com.yoco.commons.dataservices.objectify.OfyService;
import com.yoco.commons.entity.TaskJDO;

public class TaskImpl extends OfyService implements TaskDao {

    public static TaskImpl getTaskImplInstance(){
        return new TaskImpl();
    }

    @Override
    public TaskJDO getEntryByID(String id) {
        return get(TaskJDO.class, id);
    }

    @Override
    public void deleteTask(TaskJDO objTask) {
        delete(objTask);
    }

    @Override
    public TaskJDO saveTask(TaskJDO objTask) {
        save(objTask);
        return objTask;
    }

}
