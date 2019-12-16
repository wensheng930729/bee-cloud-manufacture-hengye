package com.bee.platform.cloud.user.controller;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.user.entity.ArUser;
import com.bee.platform.cloud.user.service.ArUserService;
import com.bee.platform.common.annotation.NotIntercept;
import com.bee.platform.common.entity.Config;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.SystemCode;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.SystemCodeService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * @ClassName BeatController
 * @Description 用于测试的Controller
 * @author zhigang.zhou
 * @Date 2019年09月16日 下午13:31:35
 * @version 1.0.0
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "/beat", tags = "platform-cloudmanufactureuser模块测试接口")
public class BeatController {
    
	@Autowired
	private ArUserService arUserService;
	
	@Autowired
    private RedisTemplate<Object, Object> redisTemplate;
    
	@Autowired
	private ConfigService configService;
	
	@Autowired
	private SystemCodeService systemCodeService;
	

    /**
     * @Description 检查服务是否存活
     */
    @NotIntercept
    @ApiOperation(value = "检查服务是否存活",notes ="检查服务是否存活")
    @GetMapping("/beat")
    public ResponseResult<String> beat() {
        log.info("platform-cloudmanufactureuser-beat被调用了");
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, "alive");
    }


    @NotIntercept
    @ApiOperation(value = "测试数据库的连通性",notes ="测试数据库的连通性getArUsers")
    @GetMapping("/getArUsers")
    public ResponseResult<List<ArUser>> getArUsers() {
    	List<ArUser> list = arUserService.selectList(new EntityWrapper<ArUser>());
    	return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }
    
    /**
     * @Description 测试RedisTemplate
     */
    @NotIntercept
    @ApiOperation(value = "测试RedisTemplate",notes ="测试RedisTemplate")
    @GetMapping("/test-redisTemplate")
    public ResponseResult<String> rediscluster() {
    	redisTemplate.opsForValue().set("redisTemplate-key-test", "hello RedisTemplate");
        String res = (String) redisTemplate.opsForValue().get("redisTemplate-key-test");
        log.debug("测试RedisCluster,res={}", res);
        String resStr = "res=" + res;
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resStr);
    }
    
	/**
	 * @Description 测试公共配置
	 */
	@ApiOperation(value = "根据键获取对应的配置", notes = "获取配置")
	@GetMapping(value = "/getConfigByconfigKey")
	public ResponseResult<Config> getConfigByconfigKey(String configKey) {
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, configService.getConfigByconfigKey(configKey));
	}
	
	/**
	 * @Description 测试公共码表信息的查询
	 */
	@ApiOperation(value = "通过缓存的键和码表组id获取码表信息", notes = "获取码表的信息")
	@GetMapping(value = "/getCacheSysCodeInfo")
	public ResponseResult<List<SystemCode>> getCacheSysCodeInfo(String redisKey, String codeTypeId) {
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
				systemCodeService.getCacheSysCodeInfo(redisKey, codeTypeId));
	}

}
