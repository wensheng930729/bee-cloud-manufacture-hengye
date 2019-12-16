package com.bee.platform.cloud.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.user.entity.AuthRoleResource;

import java.util.List;

/**
 * <p>
 * 资源角色(功能)表 服务类
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-24
 */
public interface AuthRoleResourceService extends IService<AuthRoleResource> {
    /**
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 18:15 2019/9/26
     * @param resources :
     * @return: void
     */
    void insertAll(List<AuthRoleResource> resources);
    /**
     * @notes: 从数据库中查询角色关联的资源id
     * @Author: junyang.li
     * @Date: 16:41 2019/9/24
     * @param roleId :
     * @return: java.util.List<java.lang.Integer>
     */
    List<Integer> selectResourceIdByRoleId(Integer roleId);
}
