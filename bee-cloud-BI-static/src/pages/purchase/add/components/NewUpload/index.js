import { Component } from "react";
import PropTypes from "prop-types";
import { Icon, Modal, Upload, message } from 'antd';
import styles from "./index.less";
import { domain ,api_factory_prefix} from '@/constants/prefix'

export default class NewUpload extends Component {
  state = {
    previewVisible: false,
    previewImage: null,
  };

  handlePreview = file => {
    if (file.name.indexOf(".png") === -1 && file.name.indexOf(".jpg") === -1 && file.name.indexOf(".jpeg") === -1 && file.name.indexOf(".gif") === -1) {
      return message.warning("此类文件无法预览");
    }
    this.setState({
      previewImage: file.url || file.thumbUrl,
      previewVisible: true,
    });
  };

  render() {
    const { accept, disabled, fileList, number, beforeUpload, onChange, onRemove } = this.props;
    const { previewVisible, previewImage } = this.state;
    const file = fileList || [];
    const uploadButton = (
      <div className={styles.upload}>
        <Icon style={{ fontSize: 20 }} type="plus" />
        <div>上传</div>
      </div>
    );
    return (
      <div className="clearfix">
        <Upload
          accept={accept}
          action={domain + api_factory_prefix+'/file/upload?type=2'}
          disabled={disabled}
          fileList={file}
          beforeUpload={beforeUpload}
          onChange={onChange}
          onRemove={disabled === true ? () => false : onRemove}
          onPreview={this.handlePreview}
        >
          {
            file.length < number && disabled === false ? uploadButton : null
          }
        </Upload>
        <Modal visible={previewVisible} footer={null} onCancel={() => this.setState({ previewImage: null, previewVisible: false })}>
          <img alt="example" style={{ width: '100%' }} src={previewImage} />
        </Modal>
      </div>
    );
  }
}

//限定控件传入的属性类型
NewUpload.propTypes = {
  accept: PropTypes.string, // 上传文件格式
  disabled: PropTypes.bool, // 是否禁用
  fileList: PropTypes.array, // 文件列表
  number: PropTypes.number, // 上传文件数
  beforeUpload: PropTypes.func, // 上传之前钩子函数
  onChange: PropTypes.func,  // 选中时的回调
  onRemove: PropTypes.func, // 移除文件回调
};

//设置默认属性
NewUpload.defaultProps = {
  accept: 'image/jpg, image/jpeg, image/png', //默认接受图片文件
  disabled: false, // 默认不禁用
  fileList: null, //文件列表
  number: 1, //默认最多上传一张图
  beforeUpload: (file, fileList) => {
    const isMax10M = file.size / 1024 / 1024 < 10; // 最大10M
    if (!isMax10M) {
      message.error("文件大小最大支持10M！");
    }
    return isMax10M;
  },
  onChange: () => false,
  onRemove: () => true,
};