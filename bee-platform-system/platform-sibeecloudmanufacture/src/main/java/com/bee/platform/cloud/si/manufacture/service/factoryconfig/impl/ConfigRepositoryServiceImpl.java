package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;


import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigRepositoryMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.FreeStorageDetailMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigRepositoryDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRepositorySaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRepositoryUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.StorageInventoryService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRepositoryService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProIngredientDetailService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProMaterialBatchDetailService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 仓库档案 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class ConfigRepositoryServiceImpl extends ServiceImpl<ConfigRepositoryMapper, ConfigRepository> implements ConfigRepositoryService {


    @Autowired
    private StorageInventoryService storageInventoryService;

    @Autowired
    private FreeStorageDetailMapper freeStorageDetailMapper;

    @Autowired
    private ProMaterialBatchDetailService proMaterialBatchDetailService;

    @Autowired
    private ProIngredientDetailService proIngredientDetailService;

    @Autowired
    private ConfigRepositoryMapper configRepositoryMapper;

    // .....待补充其他仓库

    /**
     * 查询仓库列表
     * @param userInfo 用户信息
     * @param page 分页对象
     * @return 仓库列表
     */
    @Override
    public ResponseResult<List<ConfigRepositoryDTO>> searchRepositoryList(AuthPlatformUserInfo userInfo,String name, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigRepository> wrapper = new EntityWrapper<ConfigRepository>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if(!StringUtils.isEmpty(name)){
            wrapper.like("name",name);
        }
        List<ConfigRepository> deviceList = baseMapper.selectPage(pagination, wrapper);
        List<ConfigRepositoryDTO> dto = BeanUtils.assemble(ConfigRepositoryDTO.class, deviceList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }
    /**
     * 保存仓库信息
     * @param userInfo 用户信息
     * @param rq 仓库信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveRepository(AuthPlatformUserInfo userInfo, ConfigRepositorySaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);

        // 校验 仓库名称 在公司下唯一
        List<ConfigRepository> exist = selectList(new EntityWrapper<ConfigRepository>()
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存仓库信息失败,设备重复,调用{}的{}方法出错", "ConfigRepositoryServiceImpl", "saveRepository()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.REPOSITORY_SAVE_FAILED_NAME_RE);
        }

        ConfigRepository configRepository = BeanUtils.copyProperties(rq, ConfigRepository.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(time);
        // 保存仓库信息
        if (!insert(configRepository)) {
            log.error("保存仓库信息失败，调用{}的{}方法出错", "ConfigRepositoryServiceImpl", "saveRepository()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.REPOSITORY_SAVE_FAILED);

        }
        return configRepository.getId();
    }
    /**
     * 修改仓库信息
     * @param userInfo 用户信息
     * @param rq 仓库信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateRepository(AuthPlatformUserInfo userInfo, ConfigRepositoryUpdateRQ rq) {


        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        Integer id = rq.getId();
        // 校验仓库是否在使用
        if(checkRepositoryUsed(id)){
            log.error("修改仓库信息失败,仓库已被使用禁止修改,调用{}的{}方法出错", "ConfigRepositoryServiceImpl", "updateRepository()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.REPOSITORY_UPDATE_FAILED_USED);
        }

        // 校验仓库名称在公司下唯一
        List<ConfigRepository> exist = selectList(new EntityWrapper<ConfigRepository>()
                .eq("enterprise_id", enterpriseId)
                .ne("id",id)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("修改仓库信息失败,设备重复,调用{}的{}方法出错", "ConfigRepositoryServiceImpl", "updateRepository()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.REPOSITORY_UPDATE_FAILED_NAME_RE);
        }

        // 校验此id的仓库数据是否存在
        List<ConfigRepository> have = selectList(new EntityWrapper<ConfigRepository>()
                .eq("enterprise_id", enterpriseId)
                .eq("id",id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改仓库信息失败,此id仓库不存在,调用{}的{}方法出错", "ConfigRepositoryServiceImpl", "updateRepository()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.REPOSITORY_UPDATE_FAILED_NO_DATA);
        }


        ConfigRepository configRepository = BeanUtils.copyProperties(rq, ConfigRepository.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改仓库信息
        if (!updateById(configRepository)) {
            log.error("修改仓库信息失败，调用{}的{}方法出错", "ConfigRepositoryServiceImpl", "updateRepository()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.REPOSITORY_UPDATE_FAILED);

        }
        return configRepository.getId();
    }

    /**
     * 校验仓库是否在使用
     * @param id id
     * @return 结果
     */
    private boolean checkRepositoryUsed(Integer id) {


        List<StorageInventory> storageInventories = storageInventoryService.selectList(new EntityWrapper<StorageInventory>()
                .eq("status", Status.TRUE.getKey())
                .eq("storage_id",id));
        if(!CollectionUtils.isEmpty(storageInventories)){
            return true;
        }

        List<FreeStorageDetail> freeStorageDetails = freeStorageDetailMapper.selectList(new EntityWrapper<FreeStorageDetail>()
               .eq("storage_id",id));
        if(!CollectionUtils.isEmpty(freeStorageDetails)){
            return true;
        }

        List<ProMaterialBatchDetail> proMaterialBatchDetails = proMaterialBatchDetailService.selectList(new EntityWrapper<ProMaterialBatchDetail>()
                .eq("status", Status.TRUE.getKey())
                .eq("warehouse_id",id));
        if(!CollectionUtils.isEmpty(proMaterialBatchDetails)){
            return true;
        }

        List<ProIngredientDetail> proIngredientDetails = proIngredientDetailService.selectList(new EntityWrapper<ProIngredientDetail>()
                .eq("status", Status.TRUE.getKey())
                .eq("warehouse_id",id));
        if(!CollectionUtils.isEmpty(proIngredientDetails)){
            return true;
        }

        return false;
    }

    /**
     * 根据id删除仓库
     * @param userInfo 用户信息
     * @param id id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteRepositoryById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        ConfigRepository exist = selectOne(new EntityWrapper<ConfigRepository>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在仓库信息
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除仓库信息，没有找到相应数据，id为："+id);
            return;
        }
        // 校验仓库是否在使用
        if(checkRepositoryUsed(id)){
            log.error("删除仓库信息失败,仓库已被使用禁止删除,调用{}的{}方法出错", "ConfigRepositoryServiceImpl", "deleteRepositoryById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.REPOSITORY_DELETED_FAILED_USED);
        }
        // 删除仓库信息
        if (!updateById(new ConfigRepository()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除仓库信息失败，调用{}的{}方法出错", "ConfigRepositoryServiceImpl", "deleteRepositoryById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.REPOSITORY_DELETED_FAILED);
        }
    }


    /**
     * 根据仓库类别查询仓库列表
     * @param userInfo 用户信息
     * @param types 类别列表 (仓库类别(0 成品 1 原料 2 配料  3五金 4其他) 空为查询全部)
     * @return 仓库列表
     */
    @Override
    public List<ConfigRepositoryDTO> getRepositoryListByType(AuthPlatformUserInfo userInfo, List<Integer> types) {
        Wrapper<ConfigRepository> wrapper = new EntityWrapper<ConfigRepository>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("status",Status.TRUE.getKey())
                .orderBy("id",false)
                .eq("factory_id", userInfo.getFactoryId())
                .eq("enterprise_id", userInfo.getOrgId());
        if(!CollectionUtils.isEmpty(types)){
            wrapper.in("type",types);
        }

        List<ConfigRepository> configRepositories = selectList(wrapper);

        return BeanUtils.assemble(ConfigRepositoryDTO.class,configRepositories);
    }


    @Override
    public ConfigRepositoryDTO getRepositoryById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        ConfigRepository dto = selectOne(new EntityWrapper<ConfigRepository>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(dto, ConfigRepositoryDTO.class);
    }

    /**
     * @notes: 查询当前用户可访问的仓库详细，暂时只返回 id 和 name 字段
     * @Author: junyang.li
     * @Date: 10:39 2019/11/26
     * @param userInfo : 当前操作人
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.ConfigRepository>
     */
    @Override
    public List<ConfigRepository> getRepositoryList(AuthPlatformUserInfo userInfo) {
        Integer factoryId = userInfo.getFactoryId();
        if(factoryId == null){
            return new ArrayList<>();
        }
        return configRepositoryMapper.getRepositoryList(factoryId);
    }
}
