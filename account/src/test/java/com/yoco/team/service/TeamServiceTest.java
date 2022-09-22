package com.yoco.team.service;

import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.commons.utils.DcmTeamUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.team.helper.TeamTaskInitiator;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.*;

class TeamServiceTest {
    @Test
    void getAllTeamsForAccount_UserNotAdmin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","contactID")).thenReturn(false);
            new TeamService().getAllTeamsForAccount("accID",100,"","contactID");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void getAllTeamsForAccount_UserAdmin_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","contactID")).thenReturn(true);
            HashMap<String,Object> expected = new HashMap<>();
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForAnAccount("accID",100,"")).thenReturn(expected);
            Assertions.assertEquals(expected,new TeamService().getAllTeamsForAccount("accID",100,"","contactID"));
        }
    }

    @Test
    void getAllTeamsForAccount_UserNotAdminAndRequesterContactNotSame_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","234")).thenReturn(false);
            new TeamService().getTeamsForUserInAnAccount("accID","123","234");
        }catch (Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void getAllTeamsForAccount_UserAdminAndRequesterContactNotSame_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","234")).thenReturn(true);
            HashMap<String,Object> expected = new HashMap<>();
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccount("accID","123")).thenReturn(expected);
            Assertions.assertEquals(expected,new TeamService().getTeamsForUserInAnAccount("accID","123","234"));
        }
    }

    @Test
    void getAllTeamsForAccount_UserNotAdminAndRequesterContactSame_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(false);
            HashMap<String,Object> expected = new HashMap<>();
            dcmUtilMockedStatic.when(()-> DcmTeamUtil.getTeamsForUserInAnAccount("accID","123")).thenReturn(expected);
            Assertions.assertEquals(expected,new TeamService().getTeamsForUserInAnAccount("accID","123","123"));
        }
    }

    @Test
    void deleteTeam_userNotAdmin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(false);
            new TeamService().deleteTeam("accID","teamID","123");
        }catch(Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void deleteTeam_nullTeam_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfo("accID","teamID")).thenReturn(null);
            new TeamService().deleteTeam("accID","teamID","123");
        }catch(Exception e){
            Assertions.assertEquals("Delete failed, Team not found.",e.getMessage());
        }
    }

    @Test
    void deleteTeam_validTeam_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class);
            MockedStatic<TeamTaskInitiator> teamTaskInitiatorMockedStatic = Mockito.mockStatic(TeamTaskInitiator.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            TeamDTO team = new TeamDTO();
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamInfo("accID","teamID")).thenReturn(team);
            new TeamService().deleteTeam("accID","teamID","123");
            dcmTeamUtilMockedStatic.verify(()->DcmTeamUtil.deleteTeam("accID","teamID"));
            teamTaskInitiatorMockedStatic.verify(()->TeamTaskInitiator.initiateTeamDeletionTask(team));
        }
    }

    @Test
    void createTeam_userNotAdmin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(false);
            new TeamService().createTeam("accID","payload","123");
        }catch(Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void createTeam_successFalseNullErrorMessage_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
        MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createTeam("accID", Map.of("name","teamName","ownerID","123","type","team"))).thenReturn(Map.of("success",false));
            new TeamService().createTeam("accID","{\"name\":\"teamName\",\"ownerID\":\"123\"}","123");
        }catch (Exception e){
            Assertions.assertEquals("Unknown error occurred", e.getMessage());
        }
    }

    @Test
    void createTeam_successFalseValidErrorMessage_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createTeam("accID", Map.of("name","teamName","ownerID","123","type","team"))).thenReturn(Map.of("success",false,"error_message","Error from api"));
            new TeamService().createTeam("accID","{\"name\":\"teamName\",\"ownerID\":\"123\"}","123");
        }catch (Exception e){
            Assertions.assertEquals("Error from api", e.getMessage());
        }
    }

    @Test
    void createTeam_successTrue_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class);
            MockedStatic<TeamTaskInitiator> teamTaskInitiatorMockedStatic = Mockito.mockStatic(TeamTaskInitiator.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            Map<String,Object> dcmGroup = new HashMap<>(){{
                put("id","teamid");
                put("ownerID","ownerid");
                put("accountID","accID");
                put("name","teamname");
                put("linkedContacts",List.of("contact1","contact2"));
            }};
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.createTeam("accID", Map.of("name","teamName","ownerID","123","type","team"))).thenReturn(Map.of("success",true,"group",dcmGroup));
            Map<String,Object> actual = new TeamService().createTeam("accID","{\"name\":\"teamName\",\"ownerID\":\"123\"}","123");
            TeamDTO expectedDTO = new TeamDTO("teamid","ownerid","accID","teamname", List.of("contact1","contact2"));
            Assertions.assertEquals(Map.of("team",expectedDTO),actual);
            teamTaskInitiatorMockedStatic.verify(()->TeamTaskInitiator.initiateTeamCreationTask(expectedDTO,List.of()));
        }
    }

    @Test
    void updateTeamInfo_userNotAdmin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(false);
            new TeamService().updateTeamInfo("accID","teamID","payload","123");
        }catch(Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void updateTeamInfo_successFalseNullErrorMessage_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamInfo("accID","teamID", Map.of("name","teamName","ownerID","123"))).thenReturn(Map.of("success",false));
            new TeamService().updateTeamInfo("accID","teamID","{\"name\":\"teamName\",\"ownerID\":\"123\"}","123");
        }catch (Exception e){
            Assertions.assertEquals("Unknown error occurred", e.getMessage());
        }
    }

    @Test
    void updateTeamInfo_successFalseValidErrorMessage_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamInfo("accID","teamID", Map.of("name","teamName","ownerID","123"))).thenReturn(Map.of("success",false,"error_message","Error from api"));
            new TeamService().updateTeamInfo("accID","teamID","{\"name\":\"teamName\",\"ownerID\":\"123\"}","123");
        }catch (Exception e){
            Assertions.assertEquals("Error from api", e.getMessage());
        }
    }

    @Test
    void updateTeamInfo_successTrue_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class);
            MockedStatic<TeamTaskInitiator> teamTaskInitiatorMockedStatic = Mockito.mockStatic(TeamTaskInitiator.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            Map<String,Object> dcmGroup = new HashMap<>(){{
                put("id","teamid");
                put("ownerID","ownerid");
                put("accountID","accID");
                put("name","teamname");
                put("linkedContacts",List.of("contact1","contact2"));
            }};
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamInfo("accID","teamID", Map.of("name","teamName","ownerID","123"))).thenReturn(Map.of("success",true,"group",dcmGroup));
            Map<String,Object> actual = new TeamService().updateTeamInfo("accID","teamID","{\"name\":\"teamName\",\"ownerID\":\"123\"}","123");
            TeamDTO expectedDTO = new TeamDTO("teamid","ownerid","accID","teamname", List.of("contact1","contact2"));
            Assertions.assertEquals(Map.of("team",expectedDTO),actual);
            teamTaskInitiatorMockedStatic.verify(()->TeamTaskInitiator.initiateTeamInfoUpdateTask(expectedDTO));
        }
    }

    @Test
    void updateTeamContacts_userNotAdmin_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(false);
            new TeamService().updateTeamContacts("accID","teamID","payload","123");
        }catch(Exception e){
            Assertions.assertEquals(COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value(),e.getMessage());
        }
    }

    @Test
    void updateTeamContacts_successFalseNullErrorMessage_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamContacts("accID","teamID", Map.of("type","add","contact",List.of(("1"))))).thenReturn(Map.of("success",false));
            new TeamService().updateTeamContacts("accID","teamID","{\"type\":\"add\",\"contact\":[\"1\"]}","123");
        }catch (Exception e){
            Assertions.assertEquals("Unknown error occurred", e.getMessage());
        }
    }

    @Test
    void updateTeamContacts_successFalseValidErrorMessage_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamContacts("accID","teamID", Map.of("type","add","contact",List.of(("1"))))).thenReturn(Map.of("success",false,"error_message","Error from api"));
            new TeamService().updateTeamContacts("accID","teamID","{\"type\":\"add\",\"contact\":[\"1\"]}","123");
        }catch (Exception e){
            Assertions.assertEquals("Error from api", e.getMessage());
        }
    }

    @Test
    void updateTeamContacts_successTrue_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class);
            MockedStatic<TeamTaskInitiator> teamTaskInitiatorMockedStatic = Mockito.mockStatic(TeamTaskInitiator.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.isUserAnAdminInAccount("accID","123")).thenReturn(true);
            Map<String,Object> dcmGroup = new HashMap<>(){{
                put("id","teamid");
                put("ownerID","ownerid");
                put("accountID","accID");
                put("name","teamname");
                put("linkedContacts",List.of("contact1","contact2"));
            }};
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamContacts("accID","teamID", Map.of("type","add","contact",List.of(("1"))))).thenReturn(Map.of("success",true,"group",dcmGroup));
            Map<String,Object> actual = new TeamService().updateTeamContacts("accID","teamID","{\"type\":\"add\",\"contact\":[\"1\"]}","123");
            TeamDTO expectedDTO = new TeamDTO("teamid","ownerid","accID","teamname", List.of("contact1","contact2"));
            Assertions.assertEquals(Map.of("team",expectedDTO),actual);
            teamTaskInitiatorMockedStatic.verify(()->TeamTaskInitiator.initiateTeamContactsUpdateTask(expectedDTO,List.of(),List.of("1"),"add"));
        }
    }

    @Test
    void removeUserFromAllTeamsInAnAccount_Exception_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamsForUserInAnAccount("accID","contactID")).thenReturn(Map.of("teams",List.of(new TeamDTO())));
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.updateTeamContacts(anyString(),anyString(),anyMap())).thenThrow(new IllegalArgumentException("exception"));
            new TeamService().removeUserFromAllTeamsInAnAccount("accID","contactID");
            dcmTeamUtilMockedStatic.verify(()->DcmTeamUtil.updateTeamContacts(anyString(),eq(""),anyMap()));
        }
    }

    @Test
    void removeUserFromAllTeamsInAnAccount_valid_test() throws NoSuchAlgorithmException, IOException {
        try(MockedStatic<DcmTeamUtil> dcmTeamUtilMockedStatic = Mockito.mockStatic(DcmTeamUtil.class)){
            dcmTeamUtilMockedStatic.when(()->DcmTeamUtil.getTeamsForUserInAnAccount("accID","contactID")).thenReturn(Map.of("teams",List.of(new TeamDTO())));
            new TeamService().removeUserFromAllTeamsInAnAccount("accID","contactID");
            dcmTeamUtilMockedStatic.verify(()->DcmTeamUtil.updateTeamContacts("accID","",Map.of("type","delete","contact",List.of("contactID"))));
        }
    }
}
