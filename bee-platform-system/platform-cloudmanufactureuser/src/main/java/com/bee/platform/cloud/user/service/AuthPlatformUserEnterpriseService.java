package com.bee.platform.cloud.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.user.dto.UserEnterpriseDTO;
import com.bee.platform.cloud.user.entity.AuthPlatformUserEnterprise;

/**
 * <p>
 * 企业与用户中间表 服务类
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
public interface AuthPlatformUserEnterpriseService extends IService<AuthPlatformUserEnterprise> {

    /**
     * @notes: 通过用户id查询用户的企业信息
     * @Author: junyang.li
     * @Date: 11:01 2019/9/19
     * @param userId : 用户id
     * @return: com.bee.platform.cloud.user.entity.AuthPlatformUserEnterprise
     */
    UserEnterpriseDTO selectEnterpriseByUserId(Integer userId);
}
