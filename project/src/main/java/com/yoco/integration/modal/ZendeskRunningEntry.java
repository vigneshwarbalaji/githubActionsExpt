package com.yoco.integration.modal;

import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.TaskImpl;
import lombok.Data;
import java.io.Serializable;
import java.util.Map;

@Data
public class ZendeskRunningEntry extends ZendeskEntry implements Serializable {
    private String status = "CLOCKED_IN";
    private String taskDescription;
    private String projectID;
    private Long inTimeMilliseconds;
    private String id;

    public ZendeskRunningEntry(){}

    public ZendeskRunningEntry(Map<String,Object> eventMap, String timezone){
        super(eventMap,timezone);
        this.setProjectID((String)eventMap.get(SchedulingKeys.CALENDAR));
        this.setInTimeMilliseconds((Long)eventMap.get(SchedulingKeys.START_TIME));
        this.setId((String)eventMap.get(SchedulingKeys.ID));
        this.setTaskDescription(TaskImpl.getTaskImplInstance().getEntryByID(this.getId()).getTaskDescription());
    }
}
