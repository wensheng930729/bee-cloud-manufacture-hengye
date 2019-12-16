package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.user.Application;
import com.bee.platform.cloud.user.dto.CustomerAccountDTO;
import com.bee.platform.cloud.user.service.AuthPlatformUserService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * @description: 用户相关的单元测试
 * @author: junyang.li
 * @create: 2019-10-24 15:19
 **/
@Slf4j
@RunWith(SpringRunner.class)
@SpringBootTest(classes={Application.class})
public class PlatformUserTest {

    @Autowired
    private AuthPlatformUserService authPlatformUserService;

    @Test
    public void  addCustomerAccount(){

        AuthPlatformUserInfo userInfo=new AuthPlatformUserInfo()
                .setId(1001)
                .setName("张三");

        CustomerAccountDTO dto=new CustomerAccountDTO()
                .setCarrier(1)
                .setEnterpriseId(1502)
                .setName("李俊杨")
                .setUsername("18581990837");
        authPlatformUserService.addCustomerAccount(userInfo,dto);
    }
}
