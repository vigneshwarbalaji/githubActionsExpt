package com.yoco.commonservices;

import com.yoco.commonservcies.ProjectTaskController;
import com.yoco.project.helper.ProjectTaskHelper;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class ProjectTaskControllerTest {

    @Test
    void test() throws IOException, ClassNotFoundException {

        try(MockedStatic<ProjectTaskHelper> projectTaskHelperMockedStatic = Mockito.mockStatic(ProjectTaskHelper.class)) {
            ProjectTaskHelper projectTaskHelper = Mockito.mock(ProjectTaskHelper.class);
            Mockito.doNothing().when(projectTaskHelper).projectOperationsTaskHandler(anyMap());
            projectTaskHelperMockedStatic.when(ProjectTaskHelper::getProjectTaskHelper).thenReturn(projectTaskHelper);

            Map<String,Object> payload = new HashMap<>();
            payload.put("key","data");
            var byteOut = new ByteArrayOutputStream();
            var out = new ObjectOutputStream(byteOut);
            out.writeObject(payload);

            new ProjectTaskController().projectTaskHandler(byteOut.toByteArray());

            Mockito.verify(projectTaskHelper).projectOperationsTaskHandler(payload);
        }
    }

}
