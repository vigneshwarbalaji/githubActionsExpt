package com.yoco.team.endpoint;

import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.team.helper.TeamTaskHandler;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

class TeamTaskControllerTest {
    @Test
    void handleTeamDeletion_test() throws IOException, ClassNotFoundException {
        try(MockedStatic<TeamTaskHandler> teamTaskHandlerMockedStatic = Mockito.mockStatic(TeamTaskHandler.class)){
            TeamDTO teamDTO = new TeamDTO();
            new TeamTaskController().handleTeamDeletion(CloudTaskUtil.convertObjectToByteArray(teamDTO));
            teamTaskHandlerMockedStatic.verify(()->TeamTaskHandler.handleTeamDeletion(teamDTO));
        }
    }

    @Test
    void handleTeamCreation_test() throws IOException, ClassNotFoundException {
        try(MockedStatic<TeamTaskHandler> teamTaskHandlerMockedStatic = Mockito.mockStatic(TeamTaskHandler.class)){
            Map<String,Object> payloadMap = new HashMap<>();
            new TeamTaskController().handleTeamCreation(CloudTaskUtil.convertObjectToByteArray(payloadMap));
            teamTaskHandlerMockedStatic.verify(()->TeamTaskHandler.handleTeamCreation(payloadMap));
        }
    }

    @Test
    void handleTeamInfoUpdate_test() throws IOException, ClassNotFoundException {
        try(MockedStatic<TeamTaskHandler> teamTaskHandlerMockedStatic = Mockito.mockStatic(TeamTaskHandler.class)){
            TeamDTO teamDTO = new TeamDTO();
            new TeamTaskController().handleTeamInfoUpdate(CloudTaskUtil.convertObjectToByteArray(teamDTO));
            teamTaskHandlerMockedStatic.verify(()->TeamTaskHandler.handleTeamInfoUpdate(teamDTO));
        }
    }

    @Test
    void handleTeamContactsUpdate_test() throws IOException, ClassNotFoundException {
        try(MockedStatic<TeamTaskHandler> teamTaskHandlerMockedStatic = Mockito.mockStatic(TeamTaskHandler.class)){
            Map<String,Object> payloadMap = new HashMap<>();
            new TeamTaskController().handleTeamContactsUpdate("add",CloudTaskUtil.convertObjectToByteArray(payloadMap));
            teamTaskHandlerMockedStatic.verify(()->TeamTaskHandler.handleTeamContactsUpdate(payloadMap,"add"));
        }
    }
}
