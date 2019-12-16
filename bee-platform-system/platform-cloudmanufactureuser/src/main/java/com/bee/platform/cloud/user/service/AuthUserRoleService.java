package com.bee.platform.cloud.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.user.dto.UserRoleParamDTO;
import com.bee.platform.cloud.user.entity.AuthUserRole;
import com.bee.platform.common.entity.AuthRoleInfo;

/**
 * <p>
 * 用户与角色/功能/应用的关联表 服务类
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
public interface AuthUserRoleService extends IService<AuthUserRole> {

    /**
     * @notes: 通过用户id和企业id 查询用户的角色信息
     * @Author: junyang.li
     * @Date: 13:14 2019/9/19
     * @param param : 参数
     * @return: com.bee.platform.cloud.user.entity.AuthUserRole
     */
    AuthRoleInfo getRoleByUserId(UserRoleParamDTO param);
}
