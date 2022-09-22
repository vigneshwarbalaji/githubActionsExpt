package com.yoco.integration.modal;

import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.ProjectJDO;
import lombok.Data;
import java.util.List;
import java.util.Map;

@Data
public class ZendeskInfoResponse {
    private String name;
    private String accountID;
    private String contactID;
    private List<ProjectJDO> projects;
    private Map<String,Object> entries;
    private String timezone;

    public ZendeskInfoResponse(){}

    public ZendeskInfoResponse(PeopleRelationJDO userPro, List<ProjectJDO> projects,Map<String,Object> entries){
        this.accountID = userPro.getUniquepin();
        this.contactID = userPro.getContactId();
        this.name = userPro.getContact().getFullName();
        this.timezone = userPro.getTimeZoneDisplayName();
        this.projects = projects;
        this.entries = entries;
    }
}
