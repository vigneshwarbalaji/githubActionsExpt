package com.yoco.commons.entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.googlecode.objectify.annotation.Entity;
import com.googlecode.objectify.annotation.Id;
import com.googlecode.objectify.annotation.Index;
import com.yoco.commons.enums.Status;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.HashUtil;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@Entity
public class Client implements Serializable {

    private static final long serialVersionUID = -7997218261508900143L;

    @Id
    private String id;

    private String name;

    @Index
    private String email;

    private String phone;

    private String company;

    @Index
    @JsonProperty("accountID")
    private String uniquePin;

    @Index
    private Long createdDate;

    @Index
    private Long deletedDate;

    @Index
    private Long dateModified;

    private String createdBy;

    private String deletedBy;

    @Index
    private String status;

    @Index
    @JsonProperty("projects")
    private List<String> projects = new ArrayList<>();

    public Client( String name, String email, String phone, String company, String uniquePin, String createdBy) throws NoSuchAlgorithmException {

        this.id = HashUtil.generateSHA256(this.email + System.currentTimeMillis());
        this.name = name;
        this.email = email;
        this.phone = phone;
        this.company = company;
        this.uniquePin = uniquePin;
        this.createdDate =  DateUtil.getCurrentTime();
        this.deletedDate =  0L;
        this.dateModified = this.createdDate;
        this.createdBy = createdBy;
        this.deletedBy = "";
        this.status = Status.ACTIVE.toString();
        this.projects = new ArrayList<>();
    }
}
