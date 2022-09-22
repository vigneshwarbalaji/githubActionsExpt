package com.yoco.team.helper;

import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.DcmTeamUtil;
import com.yoco.commons.utils.UserPROUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class TeamHelperTest {
    @Test
    void validateCreateTeamPayload_nullPayload_test(){
        try{
            TeamHelper.validateCreateTeamPayload(null,"accountID");
        }catch(Exception e){
            Assertions.assertEquals("Field name is mandatory.",e.getMessage());
        }
    }

    @Test
    void validateCreateTeamPayload_emptyPayload_test(){
        try{
            TeamHelper.validateCreateTeamPayload(Map.of(),"accountID");
        }catch(Exception e){
            Assertions.assertEquals("Field name is mandatory.",e.getMessage());
        }
    }

    @Test
    void validateCreateTeamPayload_emptyName_test(){
        try{
            TeamHelper.validateCreateTeamPayload(Map.of("name",""),"accountID");
        }catch(Exception e){
            Assertions.assertEquals("Invalid name.",e.getMessage());
        }
    }

    @Test
    void validateCreateTeamPayload_nullOwnerID_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setContactId("123");
            userPROUtilMockedStatic.when(()->UserPROUtil.getPrimaryAdmin("accountID")).thenReturn(userPro);
            Map<String,Object> payload = new HashMap(){{put("name","teamName");}};
            TeamHelper.validateCreateTeamPayload(payload,"accountID");
            Assertions.assertEquals(Map.of("name","teamName","ownerID","123","type","team"),payload);
        }
    }

    @Test
    void extractAndUpdateContactsToAdd_nullContacts_test(){
        Assertions.assertTrue(TeamHelper.extractAndUpdateContactsToAdd(Map.of()).isEmpty());
    }

    @Test
    void extractAndUpdateContactsToAdd_emptyContacts_test(){
        Assertions.assertTrue(TeamHelper.extractAndUpdateContactsToAdd(Map.of("contact", List.of())).isEmpty());
    }

    @Test
    void extractAndUpdateContactsToAdd_ContactsLessThan5_test(){
        Assertions.assertTrue(TeamHelper.extractAndUpdateContactsToAdd(Map.of("contact", List.of("1","2","3","4"))).isEmpty());
    }

    @Test
    void extractAndUpdateContactsToAdd_ContactsEqualsTo5_test(){
        Assertions.assertTrue(TeamHelper.extractAndUpdateContactsToAdd(Map.of("contact", List.of("1","2","3","4","5"))).isEmpty());
    }

    @Test
    void extractAndUpdateContactsToAdd_ContactsGreaterThan5_test(){
        Map<String,Object> payloadMap = new HashMap<>(){{put("contact", List.of("1","2","3","4","5","6","7"));}};
        Assertions.assertEquals(List.of("6","7"),TeamHelper.extractAndUpdateContactsToAdd(payloadMap));
        Assertions.assertEquals(List.of("1","2","3","4","5"),payloadMap.get("contact"));
    }

    @Test
    void validateUpdateTeamInfoPayload_nullPayload_test(){
        try{
            TeamHelper.validateUpdateTeamInfoPayload(null);
        }catch(Exception e){
            Assertions.assertEquals("Payload cannot be empty",e.getMessage());
        }
    }

    @Test
    void validateUpdateTeamInfoPayload_emptyPayload_test(){
        try{
            TeamHelper.validateUpdateTeamInfoPayload(new HashMap<>());
        }catch(Exception e){
            Assertions.assertEquals("Payload cannot be empty",e.getMessage());
        }
    }

    @Test
    void validateUpdateTeamInfoPayload_nullName_OwnerID_test(){
        try{
            TeamHelper.validateUpdateTeamInfoPayload(new HashMap<>(){{put("name",null);}});
        }catch(Exception e){
            Assertions.assertEquals("name or ownerID mandatory",e.getMessage());
        }
    }

    @Test
    void validateUpdateTeamInfoPayload_emptyName_OwnerID_test(){
        try{
            TeamHelper.validateUpdateTeamInfoPayload(new HashMap<>(){{put("name","");put("ownerID"," ");}});
        }catch(Exception e){
            Assertions.assertEquals("name or ownerID mandatory",e.getMessage());
        }
    }

    @Test
    void validateUpdateTeamInfoPayload_onlyValidName_test(){
        Map<String,Object> payloadMap = new HashMap<>(){{put("name","newName");put("extraKey","value");}};
        TeamHelper.validateUpdateTeamInfoPayload(payloadMap);
        Assertions.assertEquals(Map.of("name","newName"),payloadMap);
    }

    @Test
    void validateUpdateTeamInfoPayload_onlyValidOwnerID_test(){
        Map<String,Object> payloadMap = new HashMap<>(){{put("ownerID","123");put("name","");}};
        TeamHelper.validateUpdateTeamInfoPayload(payloadMap);
        Assertions.assertEquals(Map.of("ownerID","123"),payloadMap);
    }

    @Test
    void validateUpdateTeamInfoPayload_ValidNameOwnerID_test(){
        Map<String,Object> payloadMap = new HashMap<>(){{put("ownerID","123");put("name","newName");put("extraKey","value");}};
        TeamHelper.validateUpdateTeamInfoPayload(payloadMap);
        Assertions.assertEquals(Map.of("name","newName","ownerID","123"),payloadMap);
    }

    @Test
    void validateUpdateTeamContactsPayload_nullPayload_test(){
        try{
            TeamHelper.validateUpdateTeamContactsPayload(null);
        }catch (Exception e){
            Assertions.assertEquals("Payload cannot be empty",e.getMessage());
        }
    }

    @Test
    void validateUpdateTeamContactsPayload_emptyPayload_test(){
        try{
            TeamHelper.validateUpdateTeamContactsPayload(Map.of());
        }catch (Exception e){
            Assertions.assertEquals("Payload cannot be empty",e.getMessage());
        }
    }

    @Test
    void validateUpdateTeamContactsPayload_InvalidOperationType_test(){
        try{
            TeamHelper.validateUpdateTeamContactsPayload(Map.of("type","invalid"));
        }catch (Exception e){
            Assertions.assertEquals("Invalid type. Valid types include add, delete.",e.getMessage());
        }
    }

    @Test
    void validateUpdateTeamContactsPayload_nullContacts_test(){
        try{
            TeamHelper.validateUpdateTeamContactsPayload(Map.of("type","add"));
        }catch (Exception e){
            Assertions.assertEquals("Empty contacts.",e.getMessage());
        }
    }

    @Test
    void validateUpdateTeamContactsPayload_emptyContacts_test(){
        try{
            TeamHelper.validateUpdateTeamContactsPayload(Map.of("type","add","contact",List.of()));
        }catch (Exception e){
            Assertions.assertEquals("Empty contacts.",e.getMessage());
        }
    }

    @Test
    void validateUpdateTeamContactsPayload_valid_test(){
        Assertions.assertDoesNotThrow(()->TeamHelper.validateUpdateTeamContactsPayload(Map.of("type","add","contact",List.of("1"))));
    }

    @Test
    void isOperationTypeValid_null_test(){
        Assertions.assertFalse(TeamHelper.isOperationTypeValid(null));
    }

    @Test
    void isOperationTypeValid_empty_test(){
        Assertions.assertFalse(TeamHelper.isOperationTypeValid(""));
    }

    @Test
    void isOperationTypeValid_invalid_test(){
        Assertions.assertFalse(TeamHelper.isOperationTypeValid("invalid"));
    }

    @Test
    void isOperationTypeValid_add_test(){
        Assertions.assertTrue(TeamHelper.isOperationTypeValid("add"));
    }

    @Test
    void isOperationTypeValid_delete_test(){
        Assertions.assertTrue(TeamHelper.isOperationTypeValid("delete"));
    }

    @Test
    void batchDeleteContactsFromTeam_validTest_test(){
        try(MockedStatic<TeamHelper> teamHelperMockedStatic = Mockito.mockStatic(TeamHelper.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            List<String> mockContactsList = new ArrayList<>(){{add("1");add("2");add("3");add("4");add("5");add("6");}};
            TeamDTO teamDTO = new TeamDTO("id","ownerID","accID","name",new ArrayList<>());
            teamHelperMockedStatic.when(()-> TeamHelper.splitContactListByDcmApiLimit(mockContactsList)).thenCallRealMethod();
            teamHelperMockedStatic.when(()-> TeamHelper.batchDeleteContactsFromTeam(teamDTO,mockContactsList)).thenCallRealMethod();
            TeamHelper.batchDeleteContactsFromTeam(teamDTO,mockContactsList);
            teamHelperMockedStatic.verify(()->TeamHelper.batchUpdateContactsToTeam(List.of("1","2","3","4","5"),Map.of("type","delete"),teamDTO));
            teamHelperMockedStatic.verify(()->TeamHelper.batchUpdateContactsToTeam(List.of("6"),Map.of("type","delete"),teamDTO));
            teamHelperMockedStatic.verify(()-> TeamHelper.removeTeamSkilletFromMembersPro(List.of(),"accID","id"));
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamInfoUpdate","teamMap",teamDTO));
        }
    }

    @Test
    void batchAddContactsFromTeam_validTest_test(){
        try(MockedStatic<TeamHelper> teamHelperMockedStatic = Mockito.mockStatic(TeamHelper.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            List<String> mockContactsList = new ArrayList<>(){{add("1");add("2");add("3");add("4");add("5");}};
            TeamDTO teamDTO = new TeamDTO("id","ownerID","accID","name",new ArrayList<>());
            teamHelperMockedStatic.when(()-> TeamHelper.splitContactListByDcmApiLimit(mockContactsList)).thenCallRealMethod();
            teamHelperMockedStatic.when(()-> TeamHelper.batchAddContactsToTeam(teamDTO,mockContactsList)).thenCallRealMethod();
            TeamHelper.batchAddContactsToTeam(teamDTO,mockContactsList);
            teamHelperMockedStatic.verify(()->TeamHelper.batchUpdateContactsToTeam(List.of("1","2","3","4","5"),Map.of("type","add"),teamDTO));
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamInfoUpdate","teamMap",teamDTO));
        }
    }

    @Test
    void batchUpdateContactsToTeam_Exception_Test(){
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamContacts("accID","teamID",Map.of("contact",List.of()))).thenThrow(new IllegalArgumentException("Test message"));
            TeamDTO teamDTO = new TeamDTO("teamID","ownerID","accID","name",null);
            TeamHelper.batchUpdateContactsToTeam(List.of(),new HashMap<>(),teamDTO);
        }catch (Exception e){
            Assertions.assertEquals("Test message",e.getMessage());
        }
    }

    @Test
    void batchUpdateContactsToTeam_valid_Test(){
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamContacts("accID","teamID",Map.of("contact",List.of()))).thenReturn(Map.of("group",Map.of("linkedContacts",List.of("1"))));
            TeamDTO teamDTO = new TeamDTO("teamID","ownerID","accID","name",null);
            TeamHelper.batchUpdateContactsToTeam(List.of(),new HashMap<>(),teamDTO);
            Assertions.assertEquals(List.of("1"),teamDTO.getMembers());
        }
    }

    @Test
    void removeTeamSkilletFromMembersPro_valid_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            TeamDTO teamDTO = new TeamDTO("teamID","ownerID","accID","name",List.of("1","2"));
            TeamHelper.removeTeamSkilletFromMembersPro(teamDTO);
            userPROUtilMockedStatic.verify(()->UserPROUtil.removeTeamIDFromProSkillset("accID","1","teamID"));
            userPROUtilMockedStatic.verify(()->UserPROUtil.removeTeamIDFromProSkillset("accID","2","teamID"));
        }
    }

    @Test
    void splitContactListByDcmApiLimit_emptyList_test(){
        Assertions.assertTrue(TeamHelper.splitContactListByDcmApiLimit(new ArrayList<>()).isEmpty());
    }

    @Test
    void splitContactListByDcmApiLimit_ListSizeLessThan5_test(){
        Assertions.assertEquals(List.of(List.of("1")),TeamHelper.splitContactListByDcmApiLimit(new ArrayList<>(){{add("1");}}));
    }

    @Test
    void splitContactListByDcmApiLimit_ListSizeEqualTo5_test(){
        Assertions.assertEquals(List.of(List.of("1","2","3","4","5")),TeamHelper.splitContactListByDcmApiLimit(new ArrayList<>(){{add("1");add("2");add("3");add("4");add("5");}}));
    }

    @Test
    void splitContactListByDcmApiLimit_ListSizeGreaterThan5_test(){
        Assertions.assertEquals(List.of(List.of("1","2","3","4","5"),List.of("6")),TeamHelper.splitContactListByDcmApiLimit(new ArrayList<>(){{add("1");add("2");add("3");add("4");add("5");add("6");}}));
    }
}
