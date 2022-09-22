package com.yoco.commons.entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.googlecode.objectify.annotation.Entity;
import com.googlecode.objectify.annotation.Id;
import com.googlecode.objectify.annotation.Ignore;
import com.googlecode.objectify.annotation.Index;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.HashUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Data
@NoArgsConstructor
@Entity
public class ProjectJDO implements Serializable {

    @Id
    @JsonProperty("id")
    private String projectId;

    @Index
    @JsonProperty("name")
    private String projectName;

    @Index
    @JsonProperty("accountID")
    private String uniquepin;

    private Date dateAdded;

    @Index
    private Long dateAddedLongTime;

    @Index
    @JsonProperty("deleted")
    private Boolean isDeleted;

    @Index
    private Boolean isDefault;

    private Boolean billable;

    private String currency;

    private double rate = 0.0f;

    private String plan;

    @Ignore
    private List<String> contacts;


    public ProjectJDO (String accountID, String projectName, boolean billable, String currency, double rate, String plan, String projectID ){

        this.projectId          = ObjUtils.isNullOrEmpty(projectID) ? HashUtil.generateUUID() : projectID;
        this.dateAddedLongTime  = DateUtil.getCurrentTime();
        this.dateAdded          = new Date(this.dateAddedLongTime);
        this.isDeleted          = false;
        this.projectName        = projectName;
        this.isDefault          = false;
        this.uniquepin          = accountID;
        this.billable           = billable;
        this.currency           = currency;
        this.rate               = rate;
        this.plan               = plan;
    }
}
