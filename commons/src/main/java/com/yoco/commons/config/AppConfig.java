package com.yoco.commons.config;

import com.google.cloud.datastore.DatastoreOptions;
import com.googlecode.objectify.ObjectifyFactory;
import com.googlecode.objectify.ObjectifyFilter;
import com.googlecode.objectify.ObjectifyService;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.entity.*;
import com.yoco.commons.enums.AppMode;
import com.yoco.commons.utils.GaeUtils;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

@Configuration
public class AppConfig{

    @Bean
    public FilterRegistrationBean<ObjectifyFilter> objectifyFilterRegistration(){
        final FilterRegistrationBean<ObjectifyFilter> registration = new FilterRegistrationBean<>();
        registration.setFilter(new ObjectifyFilter());
        registration.addUrlPatterns("/*");
        registration.setOrder(1);
        return registration;
    }

    @Bean
    public ServletListenerRegistrationBean<ObjectifyListener> listenerRegistrationBean() {
        ServletListenerRegistrationBean<ObjectifyListener> bean = new ServletListenerRegistrationBean<>();
        bean.setListener(new ObjectifyListener());
        return bean;
    }

    @WebListener
    public static class ObjectifyListener implements ServletContextListener {

        @Override
        public void contextInitialized(ServletContextEvent sce) {

            if (AppMode.DEV.equals(GaeUtils.getAppMode())) {

                ObjectifyService.init(new ObjectifyFactory(
                        DatastoreOptions.newBuilder()
                                .setHost("http://localhost:8484")
                                .setProjectId("dev")
                                .build()
                                .getService()
                ));
            } else {
                ObjectifyService.init(new ObjectifyFactory(
                        DatastoreOptions.getDefaultInstance().getService()
                ));
            }

            // register entity classes here
            ObjectifyService.register(Client.class);
            ObjectifyService.register(Contact.class);
            ObjectifyService.register(FcmJDO.class);
            ObjectifyService.register(IntegrationsJDO.class);
            ObjectifyService.register(PeopleRelationJDO.class);
            ObjectifyService.register(ProjectJDO.class);
            ObjectifyService.register(ProjectPeopleJDO.class);
            ObjectifyService.register(SettingsJDO.class);
            ObjectifyService.register(TaskJDO.class);
            ObjectifyService.register(UserClockinSubStatusJDO.class);
        }

        @Override
        public void contextDestroyed (ServletContextEvent sce){
            // context is destroyed.
        }
    }


    @Bean
    public WebMvcConfigurer corsConfiguration() {

        return new WebMvcConfigurer() {
            @Override
            public void addCorsMappings(CorsRegistry registry) {
                registry.addMapping("/**")
                        .allowedMethods("*")
                        .allowedOriginPatterns("https://*")
                        .maxAge(86400);
            }
        };
    }

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http.headers().addHeaderWriter(CommonAppProperties.strictTransportSecurityHeader);
        http.csrf().disable();
        return http.build();
    }
}
