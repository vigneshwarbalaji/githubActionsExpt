package com.yoco.contact.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.annotation.declaration.ValidateTokenScopes;
import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.enums.ApiScope;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.contact.service.ProfileService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/v2/contact")
public class ProfileApi {

    private final ProfileService profileService = new ProfileService();

    @ValidateAccessToken
    @GetMapping("/profile/image/uploadLink")
    public ResponseEntity<GenericResponse> getProfileImageUploadUrl(@RequestParam String contactId,
                                                                    @RequestParam String fileName){
        var response = new GenericResponse();
        final String UPLOAD_URL = "upload_url";
        try{
            Map<String,Object> responseMap =  profileService.getProfileImageUploadUrl(contactId,fileName);
            response.setSuccess((Boolean) responseMap.get(Commons.SUCCESS));
            response.add(UPLOAD_URL, responseMap.containsKey(UPLOAD_URL) ? responseMap.get(UPLOAD_URL) : "");
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info(" Exception in fetching profile image upload url :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }


    @ValidateAccessToken
    @PutMapping("/profile/image")
    public ResponseEntity<GenericResponse> updateProfileImage(@RequestBody String payload){
        var response = new GenericResponse();
        try{
            Map<String,Object> uploadRespMap =  profileService.updateProfileImage(payload);
            if(ObjUtils.isNullOrEmpty(uploadRespMap)){
                response.setSuccess(false);
            }else{
                response.setSuccess(true);
                response.setData(uploadRespMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info(" Exception in updating profile image upload :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

    @ValidateAccessToken
    @ValidateTokenScopes(containsAny = {ApiScope.YOCOAPIS_FULLACCESS})
    @PutMapping("/profile")
    public ResponseEntity<GenericResponse> updateProfile(@RequestBody String payload, HttpServletRequest request){
        var response = new GenericResponse();
        try{
            var requesterContact = AnnotationHelper.extractCurrentUserContactFromRequest(request);
            Map<String,Object> updateProfileMap = profileService.updateProfile(requesterContact,payload);

            if(ObjUtils.isNullOrEmpty(updateProfileMap)){
                response.setSuccess(false);
                response.setErrorMessage(COMMON_ERROR_RESPONSE.OPERATION_FAILED.value());
            }else{
                response.setSuccess(true);
                response.setData(updateProfileMap);
            }
            return ResponseEntity.status(HttpStatus.OK).body(response);
        }catch(Exception e){
            log.info("Exception in update Profile  :: " + e.getMessage());
            response.setSuccess(false);
            response.setErrorMessage(e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
    }

}
