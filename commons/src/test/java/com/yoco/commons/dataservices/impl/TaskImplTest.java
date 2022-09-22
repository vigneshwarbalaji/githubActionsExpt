package com.yoco.commons.dataservices.impl;

import com.yoco.commons.dataservices.objectify.LocalDatastoreExtension;
import com.yoco.commons.dataservices.objectify.ObjectifyExtension;
import com.yoco.commons.entity.TaskJDO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static com.yoco.commons.dataservices.objectify.OfyService.ofy;

@ExtendWith({
        LocalDatastoreExtension.class,
        ObjectifyExtension.class})
class TaskImplTest {

    TaskImpl taskImplInstance = TaskImpl.getTaskImplInstance();

    @Test
    void getEntryByID_null_test(){
        Assertions.assertEquals(null,taskImplInstance.getEntryByID(null));
    }

    @Test
    void getEntryByID_validEntity_test(){
        TaskJDO taskJDO = new TaskJDO();
        taskJDO.setId("entryId");
        taskImplInstance.save(taskJDO);
        Assertions.assertEquals(taskJDO,taskImplInstance.getEntryByID("entryId"));
        ofy().delete().entity(taskJDO).now();
    }
}
