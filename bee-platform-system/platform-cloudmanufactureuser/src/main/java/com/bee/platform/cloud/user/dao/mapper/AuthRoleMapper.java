package com.bee.platform.cloud.user.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.user.dto.AuthRoleDTO;
import com.bee.platform.cloud.user.dto.AuthRoleParam;
import com.bee.platform.cloud.user.entity.AuthRole;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 角色表 Mapper 接口
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
public interface AuthRoleMapper extends BaseMapper<AuthRole> {

    /**
     * @notes: 条件查询角色列表
     * @Author: junyang.li
     * @Date: 17:03 2019/9/19
     * @param cloudMafType : 子应用标识
     * @return: java.util.List<com.bee.platform.cloud.user.dto.AuthRoleDTO>
     */
    List<AuthRoleDTO> selectRoleList(@Param("cloudMafType") String cloudMafType);
    /**
     * @notes: 条件查询角色列表
     * @Author: junyang.li
     * @Date: 17:03 2019/9/19
     * @return: java.util.List<com.bee.platform.cloud.user.dto.AuthRoleDTO>
     */
    List<AuthRoleDTO> selectByKeyword(AuthRoleParam param, Pagination pagination);
    /**
     * @notes: 参数查询
     * @Author: junyang.li
     * @Date: 18:21 2019/10/22
     * @param param : 参数
     * @return: java.util.List<com.bee.platform.cloud.user.entity.AuthRole>
     */
    List<AuthRole> selectByParam(AuthRoleParam param);
}
