package com.yoco.integration.modal;

import com.yoco.commons.entity.IntegrationsJDO;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import java.io.Serializable;
import java.util.HashMap;

@Data
public class IntegrationsEntityResponse implements Serializable {
    private String accountID;
    private String contactID;
    private String detailsJson;
    private String id;
    private String type;
    private Boolean enabled;
    private Long dateAdded;

    public IntegrationsEntityResponse(IntegrationsJDO integrationsJDO){
        this.setAccountID(integrationsJDO.getUniquePin());
        this.setContactID(integrationsJDO.getContactID());
        this.setDateAdded(integrationsJDO.getDateAddedLongTime());
        this.setEnabled(integrationsJDO.getIsActive());
        this.setId(integrationsJDO.getIntegrationID());
        this.setType(integrationsJDO.getIntegrationType());
        if(ObjUtils.isNull(integrationsJDO.getIntegrationDetails()) || ObjUtils.isNullOrEmpty(integrationsJDO.getIntegrationDetails())){
            this.setDetailsJson(JsonUtil.getJson(new HashMap<>()));
        }else{
            this.setDetailsJson(integrationsJDO.getIntegrationDetails());
        }

    }

}
