package com.bee.platform.cloud.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.user.entity.AuthResource;
import com.bee.platform.common.constants.enums.PlatformType;
import com.bee.platform.common.entity.AuthResourceInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 资源表 服务类
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-20
 */
public interface AuthResourceService extends IService<AuthResource> {
    /**
     * @notes: 获得所有的功能id
     * @Author: junyang.li
     * @Date: 14:50 2019/11/5
     * @return: java.util.List<java.lang.Integer>
     */
    List<Integer> getAllFunctionIds();
    /**
     * @notes: 查询指子应用的所有资源
     * @Author: junyang.li
     * @Date: 16:43 2019/9/26
     * @param platform : 应用标识
     * @return: java.util.List<com.bee.platform.cloud.user.entity.AuthResource>
     */
    List<AuthResource> selectAuthResource(PlatformType platform);
    /**
     * @notes: 通过角色id查询对应的功能
     * @Author: junyang.li
     * @Date: 14:33 2019/9/24
     * @param roleId : 角色id
     * @return: com.bee.platform.cloud.user.dto.FunctionDTO
     */
    List<AuthResourceInfo> getResourceByRoleId(Integer roleId);
    /**
     * @notes: 通过角色id查询对应的功能
     * @Author: junyang.li
     * @Date: 10:06 2019/9/25
     * @param roleId : 角色id
     * @param cloudMafType : 子应用id
     * @return: java.util.List<com.bee.platform.common.entity.AuthResourceInfo>
     */
    List<AuthResourceInfo> getResourceByRoleId(Integer roleId,String cloudMafType);
    /**
     * @notes: 从数据中查询 角色对应子应用的资源
     * @Author: junyang.li
     * @Date: 13:44 2019/9/25
     * @param roleId : 角色
     * @param cloudMafType : 子应用标识,可以为空
     * @return: java.util.List<com.bee.platform.cloud.user.entity.AuthResource>
     */
    List<AuthResource> selectResourceByRoleId(Integer roleId, String cloudMafType);
    /**
     * @notes:  获得所有菜单资源，并遍历成树
     * @Author: junyang.li
     * @Date: 16:55 2019/9/24
     * @return: com.bee.platform.common.entity.ResourceDTO
     */
    List<AuthResourceInfo> getResourceTree();
    /**
     * @notes: 获得对应资源，并遍历成树
     * @Author: junyang.li
     * @Date: 17:31 2019/9/24
     * @param resourceIds :
     * @return: java.util.List<com.bee.platform.common.entity.ResourceDTO>
     */
    List<AuthResourceInfo> getResourceTree(List<Integer> resourceIds);
    /**
     * @notes: 查询当前用户可访问的菜单
     * @Author: junyang.li
     * @Date: 11:10 2019/9/26
     * @param roleId : 当前用户角色id
     * @param cloudMafType : 子应用标识
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    ResponseResult<List<AuthResourceInfo>> getUserResource(Integer roleId,String cloudMafType);

}
