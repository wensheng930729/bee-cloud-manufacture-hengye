package com.bee.platform.cloud.user.service.impl;

import com.bee.platform.cloud.user.dto.AuthUserRoleDTO;
import com.bee.platform.cloud.user.dto.UserRoleParamDTO;
import com.bee.platform.cloud.user.entity.AuthUserRole;
import com.bee.platform.cloud.user.dao.mapper.AuthUserRoleMapper;
import com.bee.platform.cloud.user.service.AuthUserRoleService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthRoleInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 用户与角色/功能/应用的关联表 服务实现类
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
@Slf4j
@Service
public class AuthUserRoleServiceImpl extends ServiceImpl<AuthUserRoleMapper, AuthUserRole> implements AuthUserRoleService {


    @Autowired
    private AuthUserRoleMapper authUserRoleMapper;
    /**
     * @notes: 通过用户id和企业id 查询用户的角色信息
     * @Author: junyang.li
     * @Date: 13:14 2019/9/19
     * @param param : 参数
     * @return: com.bee.platform.cloud.user.entity.AuthUserRole
     */
    @Override
    public AuthRoleInfo getRoleByUserId(UserRoleParamDTO param) {
        //查询用户在当前应用的角色信息
        AuthUserRoleDTO authUserRoleDTO=authUserRoleMapper.selectRoleByUserId(param);
        //判空
        if(authUserRoleDTO==null){
            log.error("无法查询到用户的权限，查询参数:{}",param);
            throw new BusinessException(ResCodeEnum.NO_AUTHORITY, ExceptionMessageEnum.NO_AUTHORITY);
        }
        return BeanUtils.copyProperties(authUserRoleDTO,AuthRoleInfo.class);
    }
}
