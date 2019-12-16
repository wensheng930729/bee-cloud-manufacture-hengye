package com.bee.platform.cloud.user.dao.mapper;

import com.bee.platform.cloud.user.entity.AuthResource;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * <p>
 * 资源表 Mapper 接口
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-20
 */
public interface AuthResourceMapper extends BaseMapper<AuthResource> {

    /**
     * @notes: 通过角色id查询资源id
     * @Author: junyang.li
     * @Date: 10:15 2019/9/25
     * @param roleId : 角色id
     * @param cloudMafType : 子应用标识
     * @return: java.util.List<com.bee.platform.cloud.user.entity.AuthResource>
     */
    List<AuthResource> getResourceByRoleId(@Param("roleId") Integer roleId, @Param("cloudMafType") String cloudMafType);
    /**
     * @notes:  验证相关应用的功能是否存在，并返回存在的功能id
     * @Author: junyang.li
     * @Date: 15:54 2019/9/26
     * @param functionIds : 待校验功能id
     * @param resourceType : 角色类型，可为空
     * @param subSys : 应用类型，可为空
     * @return: java.util.List<java.lang.Integer>
     */
    List<AuthResource> checkFunctionIds(@Param("data")List<Integer> functionIds,
                                   @Param("resourceType")String resourceType,
                                   @Param("subSys")String subSys);

    /**
     * @notes: 通过位置查询资源id
     * @Author: junyang.li
     * @Date: 17:31 2019/9/26
     * @param position : 位置集合
     * @return: java.util.List<java.lang.Integer>
     */
    List<Integer> getResourceIdByPosition(@Param("data")Set<String> position);
}
