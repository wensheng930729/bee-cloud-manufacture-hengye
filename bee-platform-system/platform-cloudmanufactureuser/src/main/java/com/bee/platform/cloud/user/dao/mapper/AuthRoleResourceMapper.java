package com.bee.platform.cloud.user.dao.mapper;

import com.bee.platform.cloud.user.entity.AuthRoleResource;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 资源角色(功能)表 Mapper 接口
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-24
 */
public interface AuthRoleResourceMapper extends BaseMapper<AuthRoleResource> {
    /**
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 18:17 2019/9/26
     * @param resource :
     * @return: void
     */
    void insertAll(@Param("data") List<AuthRoleResource> resource);
    /**
     * @notes: 通过角色id查询资源id
     * @Author: junyang.li
     * @Date: 18:42 2019/9/26
     * @param roleId : 角色id
     * @return: java.util.List<java.lang.Integer>
     */
    List<Integer>  getResourceIdsByRoleId(Integer roleId);
}
