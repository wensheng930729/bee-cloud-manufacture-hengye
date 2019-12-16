package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigLookBoardBiMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigLookBoardBiDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigLookBoardBiEnableDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigLookBoardBiTreeDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigLookBoardBi;
import com.bee.platform.cloud.si.manufacture.rq.ConfigLookBoardBiUpdateStatusRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigLookBoardBiService;
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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 看板BI配置表 服务实现类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@Slf4j
@Service
public class ConfigLookBoardBiServiceImpl extends ServiceImpl<ConfigLookBoardBiMapper, ConfigLookBoardBi> implements ConfigLookBoardBiService {

    /**
     * 条件查询看板BI配置
     *
     * @param userInfo 用户信息
     * @param name     看板名称
     * @return 看板BI配置列表
     */
    @Override
    public ResponseResult<List<ConfigLookBoardBiDTO>> searchLookBoardBiList(AuthPlatformUserInfo userInfo, String name, Page page) {
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();

        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigLookBoardBi> wrapper1 = new EntityWrapper<ConfigLookBoardBi>()
                .eq("type", Status.FALSE.getKey())
                .eq("level", 3)
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", true);
        if (!StringUtils.isEmpty(name)) {
            wrapper1.like("name", name);
        }
        // 查询所有看板BI配置
        List<ConfigLookBoardBi> all = baseMapper.selectPage(pagination, wrapper1);

        List<String> allCodes = all.stream().map(o -> o.getCode()).collect(Collectors.toList());

        // 查询用户停用的看板BI配置
        Wrapper<ConfigLookBoardBi> wrapper2 = new EntityWrapper<ConfigLookBoardBi>()
                .eq("type", Status.TRUE.getKey())
                .eq("level", 3)
                .in("code",allCodes)
                .eq("status", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id", factoryId);

        List<ConfigLookBoardBi> stop = baseMapper.selectList(wrapper2);
        // 用户有禁用的进行替换处理
        if (!CollectionUtils.isEmpty(stop)) {
            List<String> codes = stop.stream().map(o -> o.getCode()).collect(Collectors.toList());

            List<ConfigLookBoardBi> list = all.stream().filter(o -> !codes.contains(o.getCode())).collect(Collectors.toList());

            list.addAll(stop);

            List<ConfigLookBoardBiDTO> dto = BeanUtils.assemble(ConfigLookBoardBiDTO.class, list);

            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

        }

        List<ConfigLookBoardBiDTO> dto = BeanUtils.assemble(ConfigLookBoardBiDTO.class, all);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 根据登陆信息查询用户企业启用的看板BI配置
     *
     * @param userInfo 用户信息
     * @return 看板BI列表
     */
    @Override
    public List<ConfigLookBoardBiEnableDTO> getEnableLookBoardBiList(AuthPlatformUserInfo userInfo, String type) {
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();


        Integer pid;
        switch (type) {
            // 数据概览
            case "data_overview":
                pid = 2;
                break;
            // 采购
            case "purchase":
                pid = 4;
                break;
            // 生产
            case "produce":
                pid = 11;
                break;
            // 销售
            case "sale":
                pid = 22;
                break;
            // 库存
            case "stock":
                pid = 29;
                break;
            // 匹配不上报错
            default:
                log.info("请求参数错误，没有找到相关匹配");
                throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.LOOK_BOARD_BI_GET_FAILED);

        }

        Wrapper<ConfigLookBoardBi> wrapper1 = new EntityWrapper<ConfigLookBoardBi>()
                .eq("type", Status.FALSE.getKey())
                .eq("pid", pid)
                .eq("level", 3)
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", true);
        // 查询所有看板BI配置
        List<ConfigLookBoardBi> all = baseMapper.selectList(wrapper1);

        // 查询用户停用的看板BI配置
        Wrapper<ConfigLookBoardBi> wrapper2 = new EntityWrapper<ConfigLookBoardBi>()
                .eq("type", Status.TRUE.getKey())
                .eq("pid", pid)
                .eq("level", 3)
                .eq("status", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id", factoryId);

        List<ConfigLookBoardBi> stop = baseMapper.selectList(wrapper2);
        // 用户有禁用的进行移除处理
        if (!CollectionUtils.isEmpty(stop)) {
            List<String> codes = stop.stream().map(o -> o.getCode()).collect(Collectors.toList());

            List<ConfigLookBoardBi> list = all.stream().filter(o -> !codes.contains(o.getCode())).collect(Collectors.toList());

            return BeanUtils.assemble(ConfigLookBoardBiEnableDTO.class, list);
        }

        return BeanUtils.assemble(ConfigLookBoardBiEnableDTO.class, all);

    }

    /**
     * 停用看板BI配置
     *
     * @param userInfo 用户信息
     * @param rq       请求参数
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer stopLookBoardBi(AuthPlatformUserInfo userInfo, ConfigLookBoardBiUpdateStatusRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        Integer id = rq.getId();

        // 生成一条企业下停用的记录
        Wrapper<ConfigLookBoardBi> wrapper1 = new EntityWrapper<ConfigLookBoardBi>()
                .eq("type", Status.FALSE.getKey())
                .eq("level", 3)
                .eq("deleted", Status.FALSE.getKey())
                .eq("id", id);
        ConfigLookBoardBi exist = selectOne(wrapper1);
        if (ObjectUtils.isEmpty(exist)) {
            log.error("停用看板BI配置失败,没有查询到该id的数据,调用{}的{}方法出错", "ConfigLookBoardBiServiceImpl", "stopLookBoardBi()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.LOOK_BOARD_BI_UPDATE_FAILED_NO_DATA);
        }
        ConfigLookBoardBi stop = BeanUtils.copyProperties(exist, ConfigLookBoardBi.class);
        stop.setId(null)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setStatus(Status.FALSE.getKey())
                .setType(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey())
                .setModifier(userName)
                .setModifyId(userId)
                .setModifyTime(time);

        // 保存停用看板BI配置
        if (!insert(stop)) {
            log.error("保存停用看板BI配置失败，调用{}的{}方法出错", "ConfigLookBoardBiServiceImpl", "stopLookBoardBi()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.LOOK_BOARD_BI_SAVE_FAILED);

        }

        return id;
    }

    /**
     * 启用看板BI配置
     *
     * @param userInfo 用户信息
     * @param rq       请求参数
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer enableLookBoardBi(AuthPlatformUserInfo userInfo, ConfigLookBoardBiUpdateStatusRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        Integer id = rq.getId();


        // 查询用户停用的看板BI配置
        Wrapper<ConfigLookBoardBi> wrapper = new EntityWrapper<ConfigLookBoardBi>()
                .eq("id", id)
                .eq("type", Status.TRUE.getKey())
                .eq("level", 3)
                .eq("status", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id", factoryId);
        ConfigLookBoardBi stop = selectOne(wrapper);
        if (ObjectUtils.isEmpty(stop)) {
            log.error("启用看板BI配置失败,没有查询到该id的数据,调用{}的{}方法出错", "ConfigLookBoardBiServiceImpl", "enableLookBoardBi()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.LOOK_BOARD_BI_UPDATE_FAILED_NO_DATA);
        }
        stop.setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);

        // 启用看板BI配置
        if (!updateById(stop)) {
            log.error("启用看板BI配置失败，调用{}的{}方法出错", "ConfigLookBoardBiServiceImpl", "enableLookBoardBi()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.LOOK_BOARD_BI_ENABLE_FAILED);
        }

        return id;
    }

    /**
     * 查询企业启用的看板BI完整树
     * @param userInfo 用户信息
     * @return BI完整树
     */
    @Override
    public List<ConfigLookBoardBiTreeDTO> getLookBoardBiTree(AuthPlatformUserInfo userInfo) {
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        Wrapper<ConfigLookBoardBi> wrapper1 = new EntityWrapper<ConfigLookBoardBi>()
                .eq("type", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", true);

        // 查询所有报表配置
        List<ConfigLookBoardBi> all = baseMapper.selectList(wrapper1);

        // 查询用户停用的报表配置
        Wrapper<ConfigLookBoardBi> wrapper2 = new EntityWrapper<ConfigLookBoardBi>()
                .eq("type", Status.TRUE.getKey())
                .eq("level", 3)
                .eq("status", Status.FALSE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id", factoryId);

        List<ConfigLookBoardBi> stop = baseMapper.selectList(wrapper2);
        // 用户有禁用的进行替换处理
        if (!CollectionUtils.isEmpty(stop)) {

            List<String> codes = stop.stream().map(o -> o.getCode()).collect(Collectors.toList());

            List<ConfigLookBoardBi> list = all.stream().filter(o -> !codes.contains(o.getCode())).collect(Collectors.toList());
            list.addAll(stop);

            List<ConfigLookBoardBiTreeDTO> dto = BeanUtils.assemble(ConfigLookBoardBiTreeDTO.class, list);

            return  buildByRecursive(dto);
        }

        List<ConfigLookBoardBiTreeDTO> dto = BeanUtils.assemble(ConfigLookBoardBiTreeDTO.class, all);

        return  buildByRecursive(dto);
    }










    /**
     * 使用递归方法建树
     *
     * @param treeNodes
     * @return
     */
    public static List<ConfigLookBoardBiTreeDTO> buildByRecursive(List<ConfigLookBoardBiTreeDTO> treeNodes) {
        List<ConfigLookBoardBiTreeDTO> trees = new ArrayList<>();
        for (ConfigLookBoardBiTreeDTO treeNode : treeNodes) {
            if (!ObjectUtils.isEmpty(treeNode.getPid()) && 0 == treeNode.getPid()) {
                trees.add(findChildren(treeNode, treeNodes));
            }
        }
        return trees;
    }

    /**
     * 递归查找子节点
     *
     * @param treeNodes
     * @return
     */
    public static ConfigLookBoardBiTreeDTO findChildren(ConfigLookBoardBiTreeDTO treeNode, List<ConfigLookBoardBiTreeDTO> treeNodes) {
        for (ConfigLookBoardBiTreeDTO it : treeNodes) {
            if (treeNode.getId().equals(it.getPid())) {
                if (treeNode.getChildren() == null) {
                    treeNode.setChildren(new ArrayList<>());
                }
                treeNode.getChildren().add(findChildren(it, treeNodes));
            }
        }
        return treeNode;
    }

}
