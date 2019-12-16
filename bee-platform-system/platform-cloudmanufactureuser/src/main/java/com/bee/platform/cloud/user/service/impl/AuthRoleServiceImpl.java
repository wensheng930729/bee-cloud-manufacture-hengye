package com.bee.platform.cloud.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.user.dao.mapper.AuthRoleMapper;
import com.bee.platform.cloud.user.dto.AuthRoleDTO;
import com.bee.platform.cloud.user.dto.AuthRoleParam;
import com.bee.platform.cloud.user.dto.RoleDetailDTO;
import com.bee.platform.cloud.user.entity.AuthResource;
import com.bee.platform.cloud.user.entity.AuthRole;
import com.bee.platform.cloud.user.entity.AuthRoleResource;
import com.bee.platform.cloud.user.entity.AuthUserRole;
import com.bee.platform.cloud.user.rq.CreateRoleRQ;
import com.bee.platform.cloud.user.service.AuthResourceService;
import com.bee.platform.cloud.user.service.AuthRoleResourceService;
import com.bee.platform.cloud.user.service.AuthRoleService;
import com.bee.platform.cloud.user.service.AuthUserRoleService;
import com.bee.platform.cloud.user.service.resource.AbstractResourceStrategy;
import com.bee.platform.cloud.user.service.resource.CancelResourceStrategyService;
import com.bee.platform.common.constants.enums.PlatformType;
import com.bee.platform.common.constants.enums.RoleType;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.SettlementAuthEnum;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 角色表 服务实现类
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
@Slf4j
@Service
public class AuthRoleServiceImpl extends ServiceImpl<AuthRoleMapper,AuthRole> implements AuthRoleService {

    @Autowired
    private AuthRoleMapper authRoleMapper;
    @Autowired
    private AuthUserRoleService authUserRoleService;
    @Autowired
    private AuthResourceService authResourceService;
    @Autowired
    private AuthRoleResourceService authRoleResourceService;
    @Autowired
    private JedisService jedisService;
    @Autowired
    private CancelResourceStrategyService cancelResourceStrategyService;

    /**
     * @notes: 查询角色列表
     * @Author: junyang.li
     * @Date: 17:07 2019/9/19
     * @param cloudMafType : 子应用标识  可以为空
     * @return: java.util.List<com.bee.platform.cloud.user.dto.AuthRoleDTO>
     */
    @Override
    public List<AuthRoleDTO> selectRoleList(String cloudMafType) {
        return authRoleMapper.selectRoleList(cloudMafType);
    }

    /**
     * @notes: 查询角色列表
     * @Author: junyang.li
     * @Date: 17:07 2019/9/19
     * @param pagination : 分页对象
     * @return: java.util.List<com.bee.platform.cloud.user.dto.AuthRoleDTO>
     */
    @Override
    public ResponseResult<List<AuthRoleDTO>> selectByKeyword(AuthPlatformUserInfo userInfo,String roleName,Pagination pagination) {
        AuthRoleParam param=new AuthRoleParam()
                .setRoleName(roleName)
                .setEnterpriseId(userInfo.getOrgId());
        List<AuthRoleDTO> list= authRoleMapper.selectByKeyword(param,pagination);
        //遍历
        list.forEach(obj->{
            if(RoleType.canEdit(obj.getRoleType())){
                obj.setCanEdit(Status.TRUE.getKey());
            }else {
                obj.setCanEdit(Status.FALSE.getKey());
            }
        });
        Page page= PageUtils.transToPage(pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,page);
    }
    /**
     * @notes: 查询所有角色信息，并转换为map对象，键是角色id，值是角色名称
     * @Author: junyang.li
     * @Date: 15:24 2019/9/25
     * @return: java.util.Map<java.lang.Integer,java.lang.String>
     */
    @Override
    public Map<Integer, AuthRole> getAllRoleToMap() {
        List<AuthRole> list=authRoleMapper.selectList(new EntityWrapper<AuthRole>().where("deleted=0"));
        //判空
        if(CollectionUtils.isEmpty(list)){
            return new HashMap<>(1);
        }
        Map<Integer, AuthRole> map=new HashMap<>(16);
        //遍历
        list.forEach(obj->map.put(obj.getId(),obj));
        return map;
    }

    /**
     * @notes: 通过角色id查询角色
     * @Author: junyang.li
     * @Date: 15:17 2019/9/20
     * @param roleId :
     * @return: com.bee.platform.cloud.user.entity.AuthRole
     */
    @Override
    public AuthRole selectById(Integer roleId,Integer enterpriseId) {
        if(roleId!=null){
            String[] array=new String[]{RoleType.ENTERPRISE_ROLE.getCode(),
                    RoleType.ENTERPRISE_ADMIN.getCode()};
            AuthRoleParam param=new AuthRoleParam()
                    .setEnterpriseId(enterpriseId)
                    .setRoleTypes(array)
                    .setRoleId(roleId);
            List<AuthRole> list= authRoleMapper.selectByParam(param);
            if(!CollectionUtils.isEmpty(list)){
                return list.get(0);
            }
        }
        return null;
    }

    /**
     * @notes: 创建/修改角色
     * @Author: junyang.li
     * @Date: 17:20 2019/9/20
     * @param userInfo : 当前操作人
     * @param rq : 编辑参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> editRole(AuthPlatformUserInfo userInfo,CreateRoleRQ rq) {
        if(rq.getRoleId()==null){
            return this.createRole(userInfo,rq);
        }
        return this.updateRole(userInfo,rq);
    }
    /**
     * @notes: 新增角色
     * @Author: junyang.li
     * @Date: 15:41 2019/9/26
     * @param userInfo : 当前操作人
     * @param rq : 新增参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    private ResponseResult<ResCodeEnum> createRole(AuthPlatformUserInfo userInfo,CreateRoleRQ rq){
        String roleName=rq.getRoleName();
        //查询角色名称是否重复
        AuthRole authRole=authRoleMapper.selectOne(new AuthRole().setRoleName(roleName)
                .setDeleted(Status.FALSE.getKey()));
        if(authRole!=null){
            log.error("新增角色时角色名称重复，重复数据是:{}",authRole);
            return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_EXIST);
        }
        //校验资源是否正确
        List<Integer> all=this.checkFunctionIds(rq);
        //插入角色
        AuthRole role=new AuthRole().setRoleName(roleName)
                .setRoleType(RoleType.ENTERPRISE_ROLE.getCode())
                .setEnterpriseId(userInfo.getOrgId())
                .setDeleted(Status.FALSE.getKey())
                .setDescribe(rq.getDescribe())
                .setCreateUser(userInfo.getId())
                .setCreateTime(new Date())
                .setUpdateTime(new Date());
        this.insert(role);
        //未选中任何资源，则直接返回成功
        if(all.isEmpty()){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //插入成功后，再查出角色id
        role=authRoleMapper.selectOne(new AuthRole().setRoleName(roleName)
                .setDeleted(Status.FALSE.getKey()));
        Integer roleId=role.getId();
        //为角色查询资源关联关系
        List<AuthRoleResource> list=all.stream().map(obj->{
            return new AuthRoleResource()
                    .setRoleId(roleId)
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateTime(new Date())
                    .setResourceId(obj)
                    .setCreateUser(userInfo.getId())
                    .setUpdateTime(new Date());
        }).collect(Collectors.toList());
        //插入角色与资源的关联关系
        authRoleResourceService.insertAll(list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 根据传入的功能id查询功能是否正确
     * @Author: junyang.li
     * @Date: 10:45 2019/11/5
     * @param rq :
     * @return: java.util.List<java.lang.Integer>
     */
    private List<Integer> checkFunctionIds(CreateRoleRQ rq){
        List<Integer> all=new ArrayList<>();
        //app 功能不为空
        List<Integer> app =this.checkFunctionIds(rq.getApp(),PlatformType.CLOUD_MAF_APP);
        if(!CollectionUtils.isEmpty(app)){
            all.addAll(app);
        }
        //web功能不为空
        List<Integer> web=this.checkFunctionIds(rq.getWeb(),PlatformType.CLOUD_MAF_WEB);
        if(!CollectionUtils.isEmpty(web)){
            all.addAll(web);
        }
        //bi功能不为空
        List<Integer> bi=this.checkFunctionIds(rq.getBi(),PlatformType.CLOUD_MAF_BI);
        if(!CollectionUtils.isEmpty(bi)){
            all.addAll(bi);
        }
        return  all;
    }
    /**
     * @notes: 根据传入的功能id查询功能是否正确
     * @Author: junyang.li
     * @Date: 10:44 2019/11/8
     * @param functionIds : 模块自己的功能id
     * @param type : 模块类型
     * @return: java.util.List<java.lang.Integer>
     */
    private List<Integer> checkFunctionIds(List<Integer> functionIds,PlatformType type){
        if(!CollectionUtils.isEmpty(functionIds) && type!=null){
            AbstractResourceStrategy strategy=cancelResourceStrategyService.getResourceImpl(type);
            if(strategy!=null){
                return strategy.checkFunctionIds(functionIds);
            }
        }
        return null;
    }

    /**
     * @notes: 编辑角色
     * @Author: junyang.li
     * @Date: 15:41 2019/9/26
     * @param userInfo : 当前操作人
     * @param rq : 新增参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    private ResponseResult<ResCodeEnum> updateRole(AuthPlatformUserInfo userInfo,CreateRoleRQ rq){
        Integer roleId=rq.getRoleId();
        AuthRole authRole=this.selectOne(new EntityWrapper<AuthRole>().where("deleted=0 and id={0}" ,rq.getRoleId()));
        if(authRole==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_NOT_FOUND);
        }
        //非自己公司的角色 或 非企业角色  则不允许修改
        boolean result=userInfo.getOrgId().equals(authRole.getEnterpriseId());
        if(!result){
            //没有权限修改次角色
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        //如果名称发生改变
        if(!authRole.getRoleName().equals(rq.getRoleName())){
            //则查询名称是否重复
            String roleName=rq.getRoleName();
            //查询角色名称是否重复
            AuthRole other=authRoleMapper.selectOne(new AuthRole().setRoleName(roleName)
                    .setDeleted(Status.FALSE.getKey()));
            //重复
            if(other!=null){
                log.error("修改角色时角色名称重复，重复数据是:{}",authRole);
                return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_EXIST);
            }
        }
        //修改角色数据
        this.updateById(new AuthRole()
                .setId(roleId)
                .setRoleName(rq.getRoleName())
                .setDescribe(rq.getDescribe())
                .setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId()));
        //判断资源是否发生改变
        List<Integer> all=this.checkFunctionIds(rq);
        //未选择资源
        if(all.isEmpty()){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //查询该角色当前的资源id
        List<Integer> current=authRoleResourceService.selectResourceIdByRoleId(roleId);
        //元素个数相同，且包含当前已有的资源则资源无需更新
        if(all.size()==current.size() && all.containsAll(current)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //删除原有关联资源
        authRoleResourceService.delete(new EntityWrapper<AuthRoleResource>()
                .where("role_id={0}",roleId));
        //新增角色与资源关联关系
        List<AuthRoleResource> list=all.stream().map(obj->{
            return new AuthRoleResource()
                    .setRoleId(roleId)
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateTime(new Date())
                    .setUpdateTime(new Date())
                    .setResourceId(obj)
                    .setCreateUser(userInfo.getId());
        }).collect(Collectors.toList());
        //插入角色与资源的关联关系
        authRoleResourceService.insertAll(list);
        //删除缓存
        String key=ConstantsUtil.CMF_RESOURCE_ROLE_HASH_KEY+roleId;
        if(jedisService.exists(key)){
            jedisService.delKey(key);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 删除角色
     * @Author: junyang.li
     * @Date: 17:20 2019/9/20
     * @param userInfo : 当前操作人
     * @param roleId : 角色id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> deleteRole(AuthPlatformUserInfo userInfo,Integer roleId) {
        //查询角色是否存在
        AuthRole authRole=this.selectById(roleId,userInfo.getOrgId());
        //判空
        if(authRole==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_NOT_FOUND);
        }
        //非自己公司的角色 或 非企业角色  则不允许修改
        boolean result=userInfo.getOrgId().equals(authRole.getEnterpriseId()) &&
                RoleType.ENTERPRISE_ROLE.getCode().equals(authRole.getRoleType());
        if(!result){
            //没有权限修改次角色
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        //判断该角色下是否存在用户
        int count=authUserRoleService.selectCount(new EntityWrapper<AuthUserRole>()
                .where("deleted=0 and role_id={0}",roleId));
        //大于0，则不允许删除
        if(count>0){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_DELETE_ROLE);
        }
        //删除该角色
        this.updateById(new AuthRole().setDeleted(Status.TRUE.getKey())
                .setId(roleId).setUpdateTime(new Date()));
        //删除缓存
        String key=ConstantsUtil.CMF_RESOURCE_ROLE_HASH_KEY+roleId;
        if(jedisService.exists(key)){
            jedisService.delKey(key);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 通过角色id查询角色详细信息
     * @Author: junyang.li
     * @Date: 13:54 2019/9/24
     * @param roleId : 角色id
     * @return: com.bee.platform.cloud.user.dto.RoleDetailDTO
     */
    @Override
    public ResponseResult<RoleDetailDTO> getRoleDetailById(Integer roleId,Integer enterpriseId) {
        //查询角色是否存在
        AuthRole authRole=this.selectById(roleId,enterpriseId);
        //判空
        if(authRole==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_NOT_FOUND);
        }
        //查询该角色对应的权限
        List<AuthResource> resources=authResourceService.selectResourceByRoleId(roleId,null);
        List<Integer> resourceIds=resources.stream().map(AuthResource::getId).collect(Collectors.toList());
        RoleDetailDTO dto = cancelResourceStrategyService.getFunctionByRole(resourceIds);
        dto.setRoleId(authRole.getId()).setRoleName(authRole.getRoleName()).setDescribe(authRole.getDescribe());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }
    /**
     * @notes: 获得当前用户可访问的结算单类型
     * @Author: junyang.li
     * @Date: 15:06 2019/11/6
     * @param roleId :
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    @Override
    public ResponseResult<List<String>> getSettlementAuth(Integer roleId) {
        //查询角色对应的App模块的资源
        List<AuthResource> resources=authResourceService.selectResourceByRoleId(roleId,
                PlatformType.CLOUD_MAF_APP.getValue());
        if(CollectionUtils.isEmpty(resources)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>());
        }
        List<String> list=resources.stream().map(obj->{
            return SettlementAuthEnum.getValueByKey(obj.getId());
        }).filter(Objects::nonNull).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }
}
