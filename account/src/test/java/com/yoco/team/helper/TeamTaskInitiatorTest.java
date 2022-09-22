package com.yoco.team.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.modal.dcm.TeamDTO;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.List;
import java.util.Map;

class TeamTaskInitiatorTest {
    @Test
    void initiateTeamDeletionTask_valid_test() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            CommonAppProperties.setAppUrl("url");
            TeamDTO teamDTO = new TeamDTO();
            TeamTaskInitiator.initiateTeamDeletionTask(teamDTO);
            var byteArrayOutputStream = new ByteArrayOutputStream();
            new ObjectOutputStream(byteArrayOutputStream).writeObject(teamDTO);
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("updateUserPro","url/task/team/delete",byteArray));
        }
    }

    @Test
    void initiateTeamCreationTask_valid_test() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            CommonAppProperties.setAppUrl("url");
            TeamDTO teamDTO = new TeamDTO();
            TeamTaskInitiator.initiateTeamCreationTask(teamDTO, List.of());
            var byteArrayOutputStream = new ByteArrayOutputStream();
            new ObjectOutputStream(byteArrayOutputStream).writeObject(Map.of("team",teamDTO,"contactsToAdd",List.of()));
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("updateUserPro","url/task/team/create",byteArray));
        }
    }

    @Test
    void initiateTeamInfoUpdationTask_valid_test() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            CommonAppProperties.setAppUrl("url");
            TeamDTO teamDTO = new TeamDTO();
            TeamTaskInitiator.initiateTeamInfoUpdateTask(teamDTO);
            var byteArrayOutputStream = new ByteArrayOutputStream();
            new ObjectOutputStream(byteArrayOutputStream).writeObject(teamDTO);
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("updateUserPro","url/task/team/update/info",byteArray));
        }
    }

    @Test
    void initiateTeamContactsUpdateTask_valid_test() throws IOException {
        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class)){
            CommonAppProperties.setAppUrl("url");
            TeamDTO teamDTO = new TeamDTO();
            TeamTaskInitiator.initiateTeamContactsUpdateTask(teamDTO, List.of(), List.of(),"add");
            var byteArrayOutputStream = new ByteArrayOutputStream();
            new ObjectOutputStream(byteArrayOutputStream).writeObject(Map.of("team",teamDTO,"contactsToUpdate",List.of(),"updatedContacts",List.of()));
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("updateUserPro","url/task/team/contact/add",byteArray));
        }
    }
}
