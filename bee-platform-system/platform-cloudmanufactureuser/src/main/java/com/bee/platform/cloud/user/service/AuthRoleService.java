package com.bee.platform.cloud.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.user.dto.AuthRoleDTO;
import com.bee.platform.cloud.user.dto.RoleDetailDTO;
import com.bee.platform.cloud.user.entity.AuthRole;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.user.rq.CreateRoleRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 角色表 服务类
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
public interface AuthRoleService extends IService<AuthRole> {

    /**
     * @notes: 查询角色列表
     * @Author: junyang.li
     * @Date: 17:07 2019/9/19
     * @param cloudMafType : 子应用标识  可以为空
     * @return: java.util.List<com.bee.platform.cloud.user.dto.AuthRoleDTO>
     */
    List<AuthRoleDTO> selectRoleList(String cloudMafType);
    /**
     * @notes: 查询角色列表
     * @Author: junyang.li
     * @Date: 17:07 2019/9/19
     * @param pagination : 分页对象
     * @return: java.util.List<com.bee.platform.cloud.user.dto.AuthRoleDTO>
     */
    ResponseResult<List<AuthRoleDTO>> selectByKeyword(AuthPlatformUserInfo userInfo,String roleName,Pagination pagination);
    /**
     * @notes: 查询所有角色信息，并转换为map对象，键是角色id，值是角色名称
     * @Author: junyang.li
     * @Date: 15:24 2019/9/25
     * @return: java.util.Map<java.lang.Integer,java.lang.String>
     */
    Map<Integer,AuthRole> getAllRoleToMap();
    /**
     * @notes: 通过角色id查询角色
     * @Author: junyang.li
     * @Date: 15:17 2019/9/20
     * @param roleId :
     * @return: com.bee.platform.cloud.user.entity.AuthRole
     */
    AuthRole selectById(Integer roleId,Integer enterpriseId);
    /**
     * @notes: 创建/修改角色
     * @Author: junyang.li
     * @Date: 17:20 2019/9/20
     * @param userInfo : 当前操作人
     * @param rq : 编辑参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> editRole(AuthPlatformUserInfo userInfo, CreateRoleRQ rq);
    /**
     * @notes: 删除角色
     * @Author: junyang.li
     * @Date: 17:20 2019/9/20
     * @param userInfo : 当前操作人
     * @param roleId : 角色id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> deleteRole(AuthPlatformUserInfo userInfo,Integer roleId);
    /**
     * @notes: 通过角色id查询角色详细信息
     * @Author: junyang.li
     * @Date: 13:54 2019/9/24
     * @param roleId : 角色id
     * @return: com.bee.platform.cloud.user.dto.RoleDetailDTO
     */
    ResponseResult<RoleDetailDTO> getRoleDetailById(Integer roleId,Integer enterpriseId);
    /**
     * @notes: 获得当前用户可访问的结算单类型
     * @Author: junyang.li
     * @Date: 15:06 2019/11/6
     * @param roleId :
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    ResponseResult<List<String>> getSettlementAuth(Integer roleId);
}
