package com.yoco.commons.entity;

import com.googlecode.objectify.annotation.Entity;
import com.googlecode.objectify.annotation.Id;
import com.googlecode.objectify.annotation.Ignore;
import com.googlecode.objectify.annotation.Index;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.HashUtil;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@NoArgsConstructor
@Entity
public class ProjectPeopleJDO implements Serializable {

    private static final long serialVersionUID = -5057383186517767352L;

    @Id
    private String projectPeopleId ;

    @Index
    private String uniquepin;

    @Index
    private String projectId;

    @Index
    private String peopleId;

    @Index
    private Boolean isDeleted;

    Date dateAdded;

    Long dateAddedLongTime;

    @Ignore
    private  String projectName;

    public ProjectPeopleJDO ( String accountID, String projectID, String contactID, String projectName){
        this.projectPeopleId        =       HashUtil.generateUUID();
        this.dateAddedLongTime      =       DateUtil.getCurrentTime();
        this.dateAdded              =       new Date(this.dateAddedLongTime);
        this.isDeleted              =       false;
        this.uniquepin              =       accountID;
        this.projectId              =       projectID;
        this.peopleId               =       contactID;
        this.projectName            =       projectName;
    }

}
