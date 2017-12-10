
import java.io.File

fun main(args: Array<String>) {
    // Change this to read the right file
    val inputPath = "C:\\Users\\kwarta\\Documents\\Technion\\winter18\\ProgrammingLanguages\\hw3\\kotlin\\REPY"
    val outputPath = "C:\\Users\\kwarta\\Documents\\Technion\\winter18\\ProgrammingLanguages\\hw3\\kotlin\\myOut.txt"
    File(outputPath).writeBytes(completeMe(File(inputPath).readBytes()))
}

fun completeMe(input: ByteArray): ByteArray {
    val min_val = 128.toByte()
    val max_val = 154.toByte()
    input.forEachIndexed { index, byte ->
        if (byte in min_val..max_val)
            input[index] = byte.plus(96).toByte()
    }
    val output:ByteArray = input
    return output
}