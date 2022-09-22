package com.yoco.team.helper;

import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.UserPROUtil;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.util.List;
import java.util.Map;

class TeamTaskHandlerTest {
    @Test
    void handleTeamDeletion_valid_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            TeamDTO team = new TeamDTO();
            team.setId("teamID");
            team.setAccountID("accID");
            team.setMembers(List.of("c1","c2"));
            TeamTaskHandler.handleTeamDeletion(team);
            userPROUtilMockedStatic.verify(()->UserPROUtil.removeTeamIDFromProSkillset("accID","c1","teamID"));
            userPROUtilMockedStatic.verify(()->UserPROUtil.removeTeamIDFromProSkillset("accID","c2","teamID"));
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamDeleteOperation","teamMap",team));
        }
    }

    @Test
    void handleTeamCreation_nullContactsToAdd_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<TeamHelper> teamHelperMockedStatic = Mockito.mockStatic(TeamHelper.class)){
            TeamDTO team = new TeamDTO();
            team.setId("teamID");
            team.setAccountID("accID");
            TeamTaskHandler.handleTeamCreation(Map.of("team",team));
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamInfoUpdate","teamMap",team));
            teamHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleTeamCreation_emptyContactsToAdd_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<TeamHelper> teamHelperMockedStatic = Mockito.mockStatic(TeamHelper.class)){
            TeamDTO team = new TeamDTO();
            team.setId("teamID");
            team.setAccountID("accID");
            TeamTaskHandler.handleTeamCreation(Map.of("team",team,"contactsToAdd",List.of()));
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamInfoUpdate","teamMap",team));
            teamHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleTeamCreation_validContactsToAdd_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<TeamHelper> teamHelperMockedStatic = Mockito.mockStatic(TeamHelper.class)){
            TeamDTO team = new TeamDTO();
            team.setId("teamID");
            team.setAccountID("accID");
            TeamTaskHandler.handleTeamCreation(Map.of("team",team,"contactsToAdd",List.of("1")));
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamInfoUpdate","teamMap",team));
            teamHelperMockedStatic.verify(()->TeamHelper.batchAddContactsToTeam(team,List.of("1")));
        }
    }

    @Test
    void handleTeamInfoUpdate_valid_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            TeamDTO team = new TeamDTO();
            team.setId("teamID");
            team.setAccountID("accID");
            team.setMembers(List.of("c1","c2"));
            TeamTaskHandler.handleTeamInfoUpdate(team);
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamInfoUpdate","teamMap",team));
        }
    }

    @Test
    void handleTeamContactsUpdate_addOperation_emptyContacts_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<TeamHelper> teamHelperMockedStatic = Mockito.mockStatic(TeamHelper.class)){
            TeamDTO team = new TeamDTO("teamID","ownerID","accID","name",null);
            TeamTaskHandler.handleTeamContactsUpdate(Map.of("team",team,"contactsToUpdate",List.of()),"add");
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamInfoUpdate","teamMap",team));
            teamHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void handleTeamContactsUpdate_deleteOperation_emptyContacts_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<TeamHelper> teamHelperMockedStatic = Mockito.mockStatic(TeamHelper.class)){
            TeamDTO team = new TeamDTO("teamID","ownerID","accID","name",null);
            TeamTaskHandler.handleTeamContactsUpdate(Map.of("team",team,"contactsToUpdate",List.of(),"updatedContacts",List.of("1")),"delete");
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamInfoUpdate","teamMap",team));
            teamHelperMockedStatic.verify(()-> TeamHelper.removeTeamSkilletFromMembersPro(List.of("1"),"accID","teamID"));
            teamHelperMockedStatic.verifyNoMoreInteractions();
        }
    }

    @Test
    void handleTeamContactsUpdate_deleteOperation_validContacts_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<TeamHelper> teamHelperMockedStatic = Mockito.mockStatic(TeamHelper.class)){
            TeamDTO team = new TeamDTO("teamID","ownerID","accID","name",null);
            TeamTaskHandler.handleTeamContactsUpdate(Map.of("team",team,"contactsToUpdate",List.of("1"),"updatedContacts",List.of("1")),"delete");
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamInfoUpdate","teamMap",team));
            teamHelperMockedStatic.verify(()-> TeamHelper.removeTeamSkilletFromMembersPro(List.of("1"),"accID","teamID"));
            teamHelperMockedStatic.verify(()-> TeamHelper.batchDeleteContactsFromTeam(team,List.of("1")));
        }
    }

    @Test
    void handleTeamContactsUpdate_addOperation_validContacts_test(){
        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<TeamHelper> teamHelperMockedStatic = Mockito.mockStatic(TeamHelper.class)){
            TeamDTO team = new TeamDTO("teamID","ownerID","accID","name",null);
            TeamTaskHandler.handleTeamContactsUpdate(Map.of("team",team,"contactsToUpdate",List.of("1")),"add");
            rtmServiceMockedStatic.verify(()->RTMService.publishToChannel("accID","teamInfoUpdate","teamMap",team));
            teamHelperMockedStatic.verify(()-> TeamHelper.batchAddContactsToTeam(team,List.of("1")));
            teamHelperMockedStatic.verifyNoMoreInteractions();
        }
    }
}
