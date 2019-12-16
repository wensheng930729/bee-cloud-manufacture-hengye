package com.bee.platform.cloud.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.user.dao.mapper.AuthRoleResourceMapper;
import com.bee.platform.cloud.user.entity.AuthRoleResource;
import com.bee.platform.cloud.user.service.AuthRoleResourceService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 资源角色(功能)表 服务实现类
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-24
 */
@Slf4j
@Service
public class AuthRoleResourceServiceImpl extends ServiceImpl<AuthRoleResourceMapper, AuthRoleResource> implements AuthRoleResourceService {

    @Autowired
    private AuthRoleResourceMapper authRoleResourceMapper;
    /**
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 18:15 2019/9/26
     * @param resources :
     * @return: void
     */
    @Override
    public void insertAll(List<AuthRoleResource> resources) {
        authRoleResourceMapper.insertAll(resources);
    }

    /**
     * @notes: 从数据库中查询角色关联的资源id
     * @Author: junyang.li
     * @Date: 16:41 2019/9/24
     * @param roleId :
     * @return: java.util.List<java.lang.Integer>
     */
    @Override
    public List<Integer> selectResourceIdByRoleId(Integer roleId){
        return authRoleResourceMapper.getResourceIdsByRoleId(roleId);
    }
}
