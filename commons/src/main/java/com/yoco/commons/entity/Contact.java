package com.yoco.commons.entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.googlecode.objectify.annotation.Entity;
import com.googlecode.objectify.annotation.Id;
import com.googlecode.objectify.annotation.Ignore;
import com.googlecode.objectify.annotation.Index;
import com.yoco.commons.constants.StoragePublicUrl;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.HashUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

@Data
@NoArgsConstructor
@Entity
public class Contact implements Serializable {

    private static final long serialVersionUID = -47940791545922350L;

    @Id
    private String id;

    @Index
    private String emailID;

    private String firstName;

    private String lastName;

    private String photoID;

    private Long   dateAddedLongTime;

    private Long   dateModifiedLongTime;

    @Ignore
    private Boolean isPasswordPresent;

    @Ignore
    @JsonProperty("name")
    private String fullName;

    public Contact ( String id, String emailID, String firstName, String lastName, String photoID, long modifiedLongTime){
        this.id                     =       ObjUtils.isNullOrEmpty(id) ? HashUtil.generateUUID() : id;
        this.dateAddedLongTime      =       DateUtil.getCurrentTime();
        this.dateModifiedLongTime   =       ( modifiedLongTime == 0L) ? DateUtil.getCurrentTime() : modifiedLongTime;
        this.emailID                =       emailID;
        this.firstName              =       firstName;
        this.lastName               =       lastName;
        this.photoID                =       ObjUtils.isNullOrEmpty(photoID) ? StoragePublicUrl.DUMMY_USER_PIC_URL : photoID;
        this.fullName               =       this.getFullName();
    }

    public String getFullName(){
        return  this.firstName+" "+this.lastName;
    }

}