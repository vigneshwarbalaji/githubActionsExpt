package com.yoco.commons.modal.user;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.contact.ContactDTO;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;
import java.util.*;

@Slf4j
@NoArgsConstructor
@Data
public class UserDTO implements Serializable {

    private String id;
    private String contactID;
    private String accountID;
    private String emailID;
    private String parentContactID;
    private String role;
    private boolean deleted;

    @JsonProperty("isDefault")
    private boolean isDefault;

    private Long dateAdded;
    private Long dateModified;
    private String empID;

    private String timezone;
    private String zoneId;

    private Map<String,Object> skills;
    private String rfID;
    private Map<String,Object> data_view_permission;

    private ContactDTO contact;
    private Set<String> permissions;

    public UserDTO(PeopleRelationJDO userPro,Set<String> permissions){
        this.setId(userPro.getId());
        this.setContactID(userPro.getContactId());
        this.setAccountID(userPro.getUniquepin());
        this.setEmailID(userPro.getEmailID());
        this.setParentContactID(userPro.getParentContactId());
        this.setRole(userPro.getRole());
        this.setDeleted(userPro.isDelete());
        this.setDefault(userPro.isDefault());
        this.setDateAdded(userPro.getDateAddedLongTime());
        this.setDateModified(userPro.getDateModified());
        this.setEmpID(ObjUtils.isNullOrEmpty(userPro.getLogin()) ? "" : userPro.getLogin());

        this.setTimezone(userPro.getTimeZoneDisplayName());
        this.setZoneId(userPro.getTimeZone());

        this.setRfID(ObjUtils.isNullOrEmpty(userPro.getRfId()) ? "" : userPro.getRfId());

        this.setUserSkills(userPro);
        this.setUserDataProtectionSkills(userPro);
        this.setUserContact(userPro);
        this.setUserPermissions(permissions);
    }

    private void setUserSkills(PeopleRelationJDO userPro){
        if(!ObjUtils.isNullOrEmpty(userPro.getSkillsets())){
            this.setSkills(JsonUtil.convertJsonToMap(userPro.getSkillsets()));
        }
    }

    private void setUserDataProtectionSkills(PeopleRelationJDO userPro){
        if(!ObjUtils.isNullOrEmpty(userPro.getDataProtectionSkillset())){
            this.setData_view_permission(JsonUtil.convertJsonToMap(userPro.getDataProtectionSkillset()));
        }
    }

    private void setUserContact(PeopleRelationJDO userPro){
        if (!ObjUtils.isNull(userPro.getContact())) {
            this.setContact(new ContactDTO(userPro.getContact()));
        }
    }

    private void setUserPermissions(Set<String> permissions){
        if(!ObjUtils.isNullOrEmpty(permissions)){
            this.setPermissions(permissions);
        }
    }

    public List<UserDTO> generateUserDTOList(List<PeopleRelationJDO> proList){

        List<UserDTO> userDTOList = new ArrayList<>();

        try {

            if (!ObjUtils.isEmptyList(proList)) {

                List<Contact> contacts = UserImpl.getUserImplInstance().getContactsByKeys(proList);

                log.info(" contacts: " + contacts.size());

                 proList.stream().forEach( pro -> {

                     if(!ObjUtils.isNullOrEmpty(pro.getContactId())){

                        var proContact = contacts.stream()
                                .filter( cont -> Objects.equals(cont.getId(),(pro.getContactId())))
                                .findFirst().orElse(null);

                        pro.setContact(proContact);

                        var userDTO = new UserDTO(pro,null);
                        userDTOList.add(userDTO);
                    }
                });

            }
        } catch (Exception e) {
            log.error(" error on generating UserDTO list : " + e.getMessage());
        }
        return userDTOList;
    }

}
