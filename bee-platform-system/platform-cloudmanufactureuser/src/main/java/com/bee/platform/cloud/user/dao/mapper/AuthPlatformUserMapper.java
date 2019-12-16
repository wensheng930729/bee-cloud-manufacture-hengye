package com.bee.platform.cloud.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.user.dto.UserDetailedDTO;
import com.bee.platform.cloud.user.entity.AuthPlatformUser;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
public interface AuthPlatformUserMapper extends BaseMapper<AuthPlatformUser> {
    /**
     * @notes: 批量插入用户
     * @Author: junyang.li
     * @Date: 10:51 2019/5/24
     * @param list : 用户集合
     * @return: void
     */
    void insertAllUser(List<AuthPlatformUser> list);
    /**
     * 通过关键词查询当前企业的用户信息
     * @param roleId 角色id
     * @param keyword 关键词
     * @param pagination 分页对象
     * @return 用户列表
     */
	List<UserDetailedDTO> selectUserByKeyword(@Param("keyword") String keyword,
                                              @Param("enterpriseId")Integer enterpriseId,
                                              @Param("roleId") Integer roleId,
                                              Pagination pagination);
	/**
	 * @notes: 通过用户id查询用户信息
	 * @Author: junyang.li
	 * @Date: 13:39 2019/11/6
	 * @param userIds : 用户id
	 * @return: java.util.List<com.bee.platform.common.entity.AuthPlatformUserInfo>
	 */
	List<AuthPlatformUserInfo> selectUserByIds(@Param("data") List<Integer> userIds);
}
