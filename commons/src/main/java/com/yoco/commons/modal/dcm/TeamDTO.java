package com.yoco.commons.modal.dcm;

import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Data
public class TeamDTO implements Serializable {
    private String id = "";
    private String ownerContactID = "";
    private String accountID = "";
    private String name = "";
    private List<String> members = new ArrayList<>();

    public TeamDTO(){}

    public TeamDTO(String id, String ownerContactID, String accountID, String name, List<String> members){
        this.id = id;
        this.ownerContactID =ownerContactID;
        this.accountID = accountID;
        this.name = name;
        this.members = ObjUtils.isNullOrEmpty(members) ? new ArrayList<>() : members;
    }

    public static List<TeamDTO> convertToTeamDTO(List<Map<String,Object>> dcmTeamMapList){
        List<TeamDTO> teamsDTO = new ArrayList<>();
        if(!ObjUtils.isNullOrEmpty(dcmTeamMapList)){
            dcmTeamMapList.forEach(team -> teamsDTO.add(convertToTeamDTO(team)));
        }
        return teamsDTO;
    }

    public static TeamDTO convertToTeamDTO(Map<String,Object> dcmTeamMap){
        return new TeamDTO((String)dcmTeamMap.get("id"),(String)dcmTeamMap.get("ownerID"),(String)dcmTeamMap.get("accountID"),(String)dcmTeamMap.get("name"),(List<String>)dcmTeamMap.get("linkedContacts"));
    }
}
