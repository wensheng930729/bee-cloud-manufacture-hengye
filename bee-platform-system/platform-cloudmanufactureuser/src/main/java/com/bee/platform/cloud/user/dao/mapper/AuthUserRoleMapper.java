package com.bee.platform.cloud.user.dao.mapper;

import com.bee.platform.cloud.user.dto.AuthUserRoleDTO;
import com.bee.platform.cloud.user.dto.UserRoleParamDTO;
import com.bee.platform.cloud.user.entity.AuthUserRole;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 用户与角色/功能/应用的关联表 Mapper 接口
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
public interface AuthUserRoleMapper extends BaseMapper<AuthUserRole> {

    /**
     * @notes: 通过用户id和企业id 查询用户的角色信息
     * @Author: junyang.li
     * @Date: 13:33 2019/9/19
     * @param param :  参数
     * @return: com.bee.platform.cloud.user.dto.AuthUserRoleDTO
     */
    AuthUserRoleDTO selectRoleByUserId(@Param("param") UserRoleParamDTO param);

}
