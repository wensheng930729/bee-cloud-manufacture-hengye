package com.bee.platform.cloud.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.user.dto.CustomerAccountDTO;
import com.bee.platform.cloud.user.dto.UserDetailedDTO;
import com.bee.platform.cloud.user.entity.AuthPlatformUser;
import com.bee.platform.cloud.user.rq.AddUserRQ;
import com.bee.platform.cloud.user.rq.EditPasswordRQ;
import com.bee.platform.cloud.user.rq.EditUserRQ;
import com.bee.platform.cloud.user.rq.UpdatePaaswordRQ;
import com.bee.platform.common.entity.*;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
public interface AuthPlatformUserService extends IService<AuthPlatformUser> {


    /**
     * @param list : 用户集合
     * @notes: 批量插入用户
     * @Author: junyang.li
     * @Date: 10:51 2019/5/24
     * @return: void
     */
    void insertAllUser(List<AuthPlatformUser> list);
    /**
     * @notes: 通过用户id查询用户信息
     * @Author: junyang.li
     * @Date: 16:44 2019/9/25
     * @param userId : 用户id
     * @return: com.bee.platform.cloud.user.entity.AuthPlatformUser
     */
    AuthPlatformUser getUserById(Integer userId);
    /**
     * @notes: 通过用户账号查询用户信息
     * @Author: junyang.li
     * @Date: 16:44 2019/9/25
     * @param username : 用户账号
     * @return: com.bee.platform.cloud.user.entity.AuthPlatformUser
     */
    AuthPlatformUser getUserByUsername(String username);
    /**
     * @notes 内部系统获取用户信息
     * @Author xin.huang
     * @Date 18:39 2018/12/11
     **/
    AuthPlatformUserInfo getUserInfo(String sysToken);

    /**
     * @notes 通过token获得用户信息
     * @Author xin.huang
     * @Date 18:39 2018/12/11
     **/
    AuthPlatformUserInfo getSelfInfo(String sysToken);

    /**
     * @notes 用于存在的用户请求验证码
     * @Author xin.huang
     * @Date 11:00 2019/3/5
     **/
    ResponseResult<ResCodeEnum> sendMessage(String phone);
    /**
     * @notes 更新后修改密码
     * @Author xin.huang
     * @Date 15:49 2019/3/5
     **/
    ResponseResult<ResCodeEnum> updatePassword(EditPasswordRQ rq);
    /**
     * @notes: 新增用户
     * @Author: junyang.li
     * @Date: 14:25 2019/9/20
     * @param rq : 新增参数
     * @param userInfo : 当前操作人
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> addUser(AuthPlatformUserInfo userInfo,AddUserRQ rq);
    /**
     * @notes: 工厂配置 - 人员管理 - 用户列表查询
     * @Author: junyang.li
     * @Date: 14:45 2019/9/25
     * @param keyword : 搜索关键词
     * @param roleId : 角色id
     * @param pagination : 分页对象
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.user.dto.UserDetailedDTO>>
     */
    ResponseResult<List<UserDetailedDTO>> getUserByKeyWord(AuthPlatformUserInfo userInfo,String keyword,Integer roleId, Pagination pagination);
    /**
     * @notes: 工厂配置中 - 启用或禁用账号
     * @Author: junyang.li
     * @Date: 15:42 2019/9/25
     * @param userInfo : 当前操作人
     * @param userId : 被操作人id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum>  updateUserStatus(AuthPlatformUserInfo userInfo,Integer userId);
    /**
     * @notes: 工厂配置，修改员工信息
     * @Author: junyang.li
     * @Date: 16:42 2019/9/25
     * @param userInfo : 当前操作人
     * @param rq : 被修改用户信息
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> updateUser(AuthPlatformUserInfo userInfo, EditUserRQ rq);
    /**
     * @notes: app 端通过token获得用户信息
     * @Author: junyang.li
     * @Date: 14:32 2019/9/26
     * @param sysToken : 当前用户token
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.AppUserInfo>
     */
    ResponseResult<AppUserInfo> getSelfInfoByApp(String sysToken);
    /**
     * @notes: 后台用户密码
     * @Author: junyang.li
     * @Date: 17:31 2019/9/25
     * @param userInfo : 当前操作人
     * @param rq : 新密码参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> updateUserPassword(AuthPlatformUserInfo userInfo, UpdatePaaswordRQ rq);
    /**
     * @notes: 新增客户账号
     * @Author: junyang.li
     * @Date: 17:31 2019/9/25
     * @param userInfo : 当前操作人
     * @param dto : 新增账号信息
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> addCustomerAccount(AuthPlatformUserInfo userInfo, CustomerAccountDTO dto);
    /**
     * @notes: 根据用户id查询用户详细
     * @Author: junyang.li
     * @Date: 13:45 2019/10/31
     * @param enterpriseId : 当前操作人企业id
     * @param userId : 待查询用户id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.user.entity.AuthPlatformUser>
     */
    ResponseResult<AuthPlatformUserInfo> getUserInfoById(Integer enterpriseId,Integer userId);
    /**
     * @notes: 内部系统通过用户id获得用户信息,返回一个map对象
     * @Author: junyang.li
     * @Date: 11:40 2019/11/6
     * @param enterpriseId :
     * @param userIds : 用户id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.AuthPlatformUserInfo>
     */
    ResponseResult<Map<Integer,AuthPlatformUserInfo>> getSimpleUsers(Integer enterpriseId, List<Integer> userIds);
}
