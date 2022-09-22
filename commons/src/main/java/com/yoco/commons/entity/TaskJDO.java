package com.yoco.commons.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.googlecode.objectify.annotation.Entity;
import com.googlecode.objectify.annotation.Id;
import com.googlecode.objectify.annotation.Unindex;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@Entity
public class TaskJDO implements Serializable {

    @Id
    @JsonProperty("entryID")
    private String id;

    @JsonProperty("taskID")
    private String taskID;

    @JsonProperty("taskDescription")
    @JsonIgnore
    @Unindex
    private String taskDescription;

    @JsonProperty("taskSource")
    private String taskSource;

    public TaskJDO(String id, String taskID, String taskDescription, String taskSource) {
        this.id = id;
        this.taskID = taskID;
        this.taskDescription = taskDescription;
        this.taskSource = taskSource;
    }

}