package com.yoco.commons.modal.user;

import com.google.appengine.api.datastore.Text;
import com.yoco.commons.utils.JsonUtil;
import lombok.Data;
import java.util.HashMap;
import java.util.Map;

@Data
public class DataProtectionSkillset {

    private Map<String, Object> profile;

    private boolean ipAddress;

    private boolean mailSubscription;

    private boolean location;

    private boolean beta;

    public DataProtectionSkillset(boolean ipAddress, boolean mailSubscription, boolean location, boolean beta, Map<String, Object> profile){
        this.ipAddress = ipAddress;
        this.mailSubscription = mailSubscription;
        this.location = location;
        this.beta = beta;
        this.profile = profile;
    }

    public static Map<String,Object> getUserDefaultProfileSkillset() {
        Map<String, Object> profileMap = new HashMap<>();
        profileMap.put("name", true);
        profileMap.put("photoID", true);
        profileMap.put("emailID", true);
        return profileMap;
    }

    public static Text getDefaultUserDataProtectionSkillset (){
        var defaultUserDataProtectionSkillset = new DataProtectionSkillset(true,true,true,true, getUserDefaultProfileSkillset());
        return new Text ( JsonUtil.getJson(defaultUserDataProtectionSkillset) );
    }

}
