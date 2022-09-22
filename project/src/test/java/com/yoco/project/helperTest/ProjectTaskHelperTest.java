package com.yoco.project.helperTest;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.ProjectJDO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.project.enums.PROJECT_ACTIVITY;
import com.yoco.project.enums.PROJECT_CHANNEL_KEYS;
import com.yoco.project.helper.ProjectFullMetricHelper;
import com.yoco.project.helper.ProjectTaskHelper;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.security.NoSuchAlgorithmException;
import java.util.*;

import static org.mockito.ArgumentMatchers.*;

class ProjectTaskHelperTest {

    ProjectTaskHelper projectTaskHelper = ProjectTaskHelper.getProjectTaskHelper();

    @Test
    void initiateProjectOperationsTaskQueue_valid_test() throws IOException {

        try(MockedStatic<TaskCreator> taskCreatorMockedStatic = Mockito.mockStatic(TaskCreator.class);
            MockedStatic<CommonAppProperties> commonAppPropertiesMockedStatic = Mockito.mockStatic(CommonAppProperties.class)){

            commonAppPropertiesMockedStatic.when(CommonAppProperties::getAppUrl).thenReturn("https://url");
            taskCreatorMockedStatic.when(() -> TaskCreator.createPostTask(anyString(),anyString(),any())).thenAnswer((Answer<Void>) invocation -> null);
            Map<String,Object> response = new HashMap<>();
            response.put("key","data");
            projectTaskHelper.initiateProjectOperationsTaskQueue(response);

            var byteOut = new ByteArrayOutputStream();
            var out = new ObjectOutputStream(byteOut);
            out.writeObject(response);

            taskCreatorMockedStatic.verify(()-> TaskCreator.createPostTask("project-operation","https://url/project/projectOperationsTaskHandler",byteOut.toByteArray()));
        }
    }

    @Test
    void projectOperationsTaskHandler_create_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic projectFullMetricHelperMockedStatic = Mockito.mockStatic(ProjectFullMetricHelper.class)){

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>)invocation -> null);

            projectFullMetricHelperMockedStatic.when(() -> ProjectFullMetricHelper.projectCreationMetric(anyString(),anyBoolean()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setUniquepin("accountId");
            projectJDO.setProjectName("project");
            projectJDO.setContacts(new ArrayList<>());
            projectJDO.setBillable(false);

            Contact contact = new Contact();
            contact.setId("contactId");
            contact.setEmailID("test@gmail.com");

            Map<String,Object> payload = new HashMap<>();
            payload.put("projectJDO",projectJDO);
            payload.put("action","create");
            payload.put("sourceClientId","web");
            payload.put("currentUser",contact);
            payload.put("currentTime",1641548481000L);

            projectTaskHelper.projectOperationsTaskHandler(payload);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("accountId", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectJDO));

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountId","contactId", PROJECT_ACTIVITY.OPERATIONS.value(),
                    "test@gmail.com","test@gmail.com has created a Project with name : project",ActivityUtil.ACTIVITIES.DUMMY.value(),1641548481000L));

            projectFullMetricHelperMockedStatic.verify(() -> ProjectFullMetricHelper.projectCreationMetric("accountId",false));
        }
    }

    @Test
    void projectOperationsTaskHandler_enable_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic projectFullMetricHelperMockedStatic = Mockito.mockStatic(ProjectFullMetricHelper.class)){

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>)invocation -> null);

            projectFullMetricHelperMockedStatic.when(() -> ProjectFullMetricHelper.projectActivationMetric(anyString()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setProjectId("projectId");
            projectJDO.setUniquepin("accountId");
            projectJDO.setProjectName("project");

            Contact contact = new Contact();
            contact.setId("contactId");
            contact.setEmailID("test@gmail.com");

            Map<String,Object> payload = new HashMap<>();
            payload.put("projectJDO",projectJDO);
            payload.put("action","enable");
            payload.put("currentUser",contact);
            payload.put("currentTime",1641548481000L);

            projectTaskHelper.projectOperationsTaskHandler(payload);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("accountId", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectJDO));

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountId","contactId", PROJECT_ACTIVITY.ENABLED.value(),
                    "test@gmail.com","projectId enabled by :: contactId",ActivityUtil.ACTIVITIES.DUMMY.value(),1641548481000L));

            projectFullMetricHelperMockedStatic.verify(() -> ProjectFullMetricHelper.projectActivationMetric("accountId"));
        }
    }

    @Test
    void projectOperationsTaskHandler_update_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic projectFullMetricHelperMockedStatic = Mockito.mockStatic(ProjectFullMetricHelper.class)){

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            projectFullMetricHelperMockedStatic.when(() -> ProjectFullMetricHelper.projectCreationMetric(anyString(),anyBoolean()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setUniquepin("accountId");
            projectJDO.setProjectName("projectTest");
            projectJDO.setContacts(new ArrayList<>());
            projectJDO.setBillable(false);

            Contact contact = new Contact();
            contact.setId("contactId");
            contact.setEmailID("newProject@gmail.com");

            List<String>associationList = new ArrayList<>();
            associationList.add("345");
            associationList.add("9487");

            List<String>disAssociationList = new ArrayList<>();
            disAssociationList.add("35");
            disAssociationList.add("87");

            List<String>intersectionList = new ArrayList<>();
            intersectionList.add("345");
            intersectionList.add("9487");

            Map<String,Object> payload = new HashMap<>();
            payload.put("projectJDO",projectJDO);
            payload.put("action","update");
            payload.put("sourceClientId","8765");
            payload.put("currentUser",contact);
            payload.put("currentTime",1676754786500L);
            payload.put("associationList", associationList);
            payload.put("disassociationList", disAssociationList);
            payload.put("intersectionList", intersectionList);

            projectTaskHelper.projectOperationsTaskHandler(payload);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("accountId", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectJDO));

        }
    }

    @Test
    void projectOperationsTaskHandler_delete_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic projectFullMetricHelperMockedStatic = Mockito.mockStatic(ProjectFullMetricHelper.class)){

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),any(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            projectFullMetricHelperMockedStatic.when(() -> ProjectFullMetricHelper.projectCreationMetric(anyString(),anyBoolean()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>)invocation -> null);

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setUniquepin("accountId");
            projectJDO.setProjectName("projectTest");
            projectJDO.setContacts(new ArrayList<>());
            projectJDO.setBillable(false);

            Contact contact = new Contact();
            contact.setId("contactId");
            contact.setEmailID("newProject@gmail.com");

            List<String>disAssociationList = new ArrayList<>();
            disAssociationList.add("35");
            disAssociationList.add("87");

            Map<String,Object> payload = new HashMap<>();
            payload.put("projectJDO",projectJDO);
            payload.put("action","delete");
            payload.put("sourceClientId","85");
            payload.put("loggedInUser",contact);
            payload.put("currentTime",167677656500L);
            payload.put("dissociationList", disAssociationList);

            projectTaskHelper.projectOperationsTaskHandler(payload);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("accountId", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectJDO));

        }
    }

    @Test
    void projectOperationsTaskHandler_else_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic projectFullMetricHelperMockedStatic = Mockito.mockStatic(ProjectFullMetricHelper.class)){

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setUniquepin("accountId");
            projectJDO.setProjectName("project");
            projectJDO.setContacts(new ArrayList<>());
            projectJDO.setBillable(false);

            Contact contact = new Contact();
            contact.setId("contactId");
            contact.setEmailID("test@gmail.com");

            Map<String,Object> payload = new HashMap<>();
            payload.put("projectJDO",projectJDO);
            payload.put("action","");
            payload.put("sourceClientId","web");
            payload.put("currentUser",contact);
            payload.put("currentTime",1641548481000L);

            projectTaskHelper.projectOperationsTaskHandler(payload);

            rtmServiceMockedStatic.verifyNoInteractions();
            activityUtilMockedStatic.verifyNoInteractions();
            projectFullMetricHelperMockedStatic.verifyNoInteractions();
        }
    }

    @Test
    void createProjectTaskHandler_test(){

        try(MockedStatic<RTMService> rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<ProjectFullMetricHelper> projectFullMetricHelperMockedStatic = Mockito.mockStatic(ProjectFullMetricHelper.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class);
            MockedStatic<FCMService> fcmServiceMockedStatic = Mockito.mockStatic(FCMService.class)){

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>)invocation -> null);

            projectFullMetricHelperMockedStatic.when(() -> ProjectFullMetricHelper.projectCreationMetric(anyString(),anyBoolean()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setProjectId("projectId");
            projectJDO.setUniquepin("accountId");
            projectJDO.setProjectName("project");

            List<String> contacts = new ArrayList<>();
            contacts.add("contactId");
            projectJDO.setContacts(contacts);
            projectJDO.setBillable(false);

            Contact contact = new Contact();
            contact.setId("contactId");
            contact.setEmailID("test@gmail.com");

            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getByID(anyString())).thenReturn(contact);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);

            FCMService fcmServiceMock = Mockito.mock(FCMService.class);
            Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            fcmServiceMockedStatic.when(FCMService::getFCMService).thenReturn(fcmServiceMock);

            projectTaskHelper.createProjectTaskHandler(projectJDO,contact,"web",1641548481000L);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("accountId", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectJDO));

            rtmServiceMockedStatic.verify(() -> RTMService.publishToAW("accountId", "contactId",PROJECT_CHANNEL_KEYS.PROJECT_ASSOCIATION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectJDO));

            Set<String> contactIDSet = new HashSet<>(projectJDO.getContacts());
            List<String> projectIDList = new ArrayList<>();
            projectIDList.add(projectJDO.getProjectId());

            Mockito.verify(fcmServiceMock).notifyFCM("accountId",contactIDSet,FCMService.FCM_SERVICE_CONSTANTS.FCM_PROJECT.value(),true,projectIDList,"web");

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountId","contactId",PROJECT_ACTIVITY.OPERATIONS.value(),
                    "test@gmail.com","test@gmail.com has created a Project with name : project and added test@gmail.com ",ActivityUtil.ACTIVITIES.DUMMY.value(),1641548481000L));

            projectFullMetricHelperMockedStatic.verify(() -> ProjectFullMetricHelper.projectCreationMetric("accountId",false));
        }
    }

    @Test
    void createProjectTaskHandler_IOException_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ContactImpl> contactMockedStatic = Mockito.mockStatic(ContactImpl.class)){

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenThrow(new IOException("exception"));

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setProjectId("projectId");
            projectJDO.setUniquepin("accountId");
            projectJDO.setProjectName("project");

            List<String> contacts = new ArrayList<>();
            contacts.add("contactId");
            projectJDO.setContacts(contacts);
            projectJDO.setBillable(false);

            Contact contact = new Contact();
            contact.setId("contactId");
            contact.setEmailID("test@gmail.com");

            ContactImpl contactMock = Mockito.mock(ContactImpl.class);
            Mockito.when(contactMock.getByID(anyString())).thenReturn(null);
            contactMockedStatic.when(ContactImpl::getContactImplInstance).thenReturn(contactMock);

            projectTaskHelper.createProjectTaskHandler(projectJDO,contact,"web",1641548481000L);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("accountId", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectJDO));
        }
    }

    @Test
    void enableProjectTaskHandler_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<ProjectFullMetricHelper> projectFullMetricHelperMockedStatic = Mockito.mockStatic(ProjectFullMetricHelper.class)){

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>)invocation -> null);

            projectFullMetricHelperMockedStatic.when(() -> ProjectFullMetricHelper.projectActivationMetric(anyString()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            ProjectJDO projectJDO = new ProjectJDO();
            projectJDO.setProjectId("projectId");
            projectJDO.setUniquepin("accountId");
            projectJDO.setProjectName("project");

            Contact contact = new Contact();
            contact.setId("contactId");
            contact.setEmailID("test@gmail.com");

            projectTaskHelper.enableProjectTaskHandler(projectJDO,contact,1641548481000L);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("accountId", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectJDO));

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("accountId","contactId",PROJECT_ACTIVITY.ENABLED.value(),
                    "test@gmail.com","projectId enabled by :: contactId",ActivityUtil.ACTIVITIES.DUMMY.value(),1641548481000L));

            projectFullMetricHelperMockedStatic.verify(() -> ProjectFullMetricHelper.projectActivationMetric("accountId"));
        }
    }

    @Test
    void updateProjectTaskHandler_exception_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedConstruction<FCMService> mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            })){

            List<String>associationList = new ArrayList<>();
            associationList.add("8787g");
            associationList.add("8778yygy");

            List<String>disAssociationList = new ArrayList<>();
            disAssociationList.add("373254");
            disAssociationList.add("8987t");

            List<String>intersectionList = new ArrayList<>();
            intersectionList.add("7769hgu");
            intersectionList.add("89836r565");

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("232");
            projectObject.setProjectName("testProject");
            projectObject.setUniquepin("5354");
            projectObject.setRate(2.00);
            projectObject.setIsDeleted(true);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenThrow(new IOException("exception"));

            projectTaskHelper.updateProjectTaskHandler(associationList, disAssociationList, intersectionList, projectObject, "432");

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("5354", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectObject));

        }

    }

    @Test
    void updateProjectTaskHandler_WithoutAssociationAndintersectionList_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedConstruction<FCMService> mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            })){

            List<String>associationList = new ArrayList<>();

            List<String>disAssociationList = new ArrayList<>();
            disAssociationList.add("37g7");
            disAssociationList.add("383y");

            List<String>intersectionList = new ArrayList<>();

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("232");
            projectObject.setProjectName("test");
            projectObject.setUniquepin("5354");
            projectObject.setRate(2.00);
            projectObject.setIsDeleted(true);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            projectTaskHelper.updateProjectTaskHandler(associationList, disAssociationList, intersectionList, projectObject, "432");

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("5354", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectObject));
        }

    }

    @Test
    void updateProjectTaskHandler_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedConstruction<FCMService> mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            })){

            List<String>associationList = new ArrayList<>();
            associationList.add("48y4");
            associationList.add("87");

            List<String>disAssociationList = new ArrayList<>();
            disAssociationList.add("37g7");
            disAssociationList.add("383y");

            List<String>intersectionList = new ArrayList<>();
            intersectionList.add("776");
            intersectionList.add("8983");

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("232");
            projectObject.setProjectName("test");
            projectObject.setUniquepin("5354");
            projectObject.setRate(2.00);
            projectObject.setIsDeleted(true);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            projectTaskHelper.updateProjectTaskHandler(associationList, disAssociationList, intersectionList, projectObject, "432");

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("5354", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectObject));
        }

    }

    @Test
    void publishingAssociateContactsToAw_test() throws IOException, NoSuchAlgorithmException {

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            List<String>associatedList = new ArrayList<>();
            associatedList.add("111");

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setUniquepin("accountID");
            projectObject.setProjectId("123");

            projectTaskHelper.publishingAssociatedContactsToAW("accountID", associatedList, "project", projectObject);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToAW("accountID", "111",PROJECT_CHANNEL_KEYS.PROJECT_ASSOCIATION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectObject));
        }

    }

    @Test
    void publishingDisAssociatedContactsToAw_test() throws IOException, NoSuchAlgorithmException {

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            List<String>associatedList = new ArrayList<>();
            associatedList.add("115");

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setUniquepin("accountID");
            projectObject.setProjectId("t65");

            projectTaskHelper.publishingDisAssociatedContactsToAW("accountID", associatedList, "project", projectObject);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToAW("accountID", "115",PROJECT_CHANNEL_KEYS.PROJECT_DISASSOCIATION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectObject));

        }

    }

    @Test
    void publishingIntersectionListToAw_test() throws IOException, NoSuchAlgorithmException {

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class)){
            List<String>intersectionList = new ArrayList<>();
            intersectionList.add("76r");

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setUniquepin("accountID");
            projectObject.setProjectId("u7y7t");

            projectTaskHelper.publishingIntersectionListToAW("accountID", intersectionList, "project", projectObject);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToAW("accountID", "76r",PROJECT_CHANNEL_KEYS.PROJECT_NAME_UPDATE.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectObject));
        }

    }

    @Test
    void deleteProjectTaskHandler_exception_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedConstruction<FCMService> mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            })){


            List<String>disAssociationList = new ArrayList<>();
            disAssociationList.add("373254");
            disAssociationList.add("8987t");

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("232");
            projectObject.setProjectName("testProject");
            projectObject.setUniquepin("5354");
            projectObject.setRate(2.00);
            projectObject.setIsDeleted(true);

            Contact contact = new Contact("243", "new@email.com", "test", "test", "31", 2345654333443L);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenThrow(new IOException("exception"));

            projectTaskHelper.deleteProjectTaskHandler(disAssociationList, projectObject, "432", contact, 876543256787L);

            activityUtilMockedStatic.verify(() -> ActivityUtil.saveActivity("5354","243",PROJECT_ACTIVITY.OPERATIONS.value(),
                    "new@email.com","new@email.com has deleted a Project with name : testProject",ActivityUtil.ACTIVITIES.DUMMY.value(),876543256787L));

        }

    }

    @Test
    void deleteProjectTaskHandler_test(){

        try(MockedStatic rtmServiceMockedStatic = Mockito.mockStatic(RTMService.class);
            MockedStatic<ActivityUtil> activityUtilMockedStatic = Mockito.mockStatic(ActivityUtil.class);
            MockedStatic<ProjectFullMetricHelper> projectFullMetricHelperMockedStatic = Mockito.mockStatic(ProjectFullMetricHelper.class);
            MockedConstruction<FCMService> mock = Mockito.mockConstruction(FCMService.class, (fcmServiceMock, context) -> {
                Mockito.doNothing().when(fcmServiceMock).notifyFCM(anyString(),anySet(),anyString(),any(boolean.class),any(),anyString());
            })){

            List<String>disAssociationList = new ArrayList<>();
            disAssociationList.add("37g7");
            disAssociationList.add("383y");

            ProjectJDO projectObject = new ProjectJDO();
            projectObject.setProjectId("232");
            projectObject.setProjectName("test");
            projectObject.setUniquepin("5354");
            projectObject.setRate(2.00);
            projectObject.setIsDeleted(true);

            Contact contact = new Contact("243", "new@email.com", "test", "test", "31", 2345654333443L);

            rtmServiceMockedStatic.when(() -> RTMService.publishToAW(anyString(),anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            rtmServiceMockedStatic.when(() -> RTMService.publishToChannel(anyString(),anyString(),anyString(),any()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            activityUtilMockedStatic.when(() -> ActivityUtil.saveActivity(anyString(),anyString(),anyString(),anyString(),
                    anyString(),anyString(),any(long.class))).thenAnswer((Answer<Void>)invocation -> null);

            projectFullMetricHelperMockedStatic.when(() -> ProjectFullMetricHelper.projectActivationMetric(anyString()))
                    .thenAnswer((Answer<Void>)invocation -> null);

            projectTaskHelper.deleteProjectTaskHandler(disAssociationList,projectObject, "432", contact, 989786548765L);

            rtmServiceMockedStatic.verify(() -> RTMService.publishToChannel("5354", PROJECT_CHANNEL_KEYS.YOCO_PROJECTS_ACTION.value(),
                    PROJECT_CHANNEL_KEYS.PROJECT_KEY.value(),projectObject));

            projectFullMetricHelperMockedStatic.verify(() -> ProjectFullMetricHelper.projectDeletionMetric("5354"));

        }

    }
}
